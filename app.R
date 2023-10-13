#  ENREP GOES Data Viewer


library(readr)
library(dplyr)
library(tidyr)
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(lubridate)
library(gt)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("ENREP Telemetry"),

sidebarLayout(
    # remove sidebar
    sidebarPanel(width = 0),
    
    mainPanel(
      tabsetPanel(
        tabPanel("GT",
                 gt_output(outputId = "gtSedTable"),
                 br(),
                 br(),
                 gt_output(outputId = "gtMetTable")
        ),
        tabPanel("DT",
          DT::dataTableOutput("metTable"),
          br(),
          br(),
          DT::dataTableOutput("sedEventTable")
        )
      ),
      br(),
      br(),
      checkboxGroupInput("plotCheckGroup", label = h3("Select Data to Plot"), 
                         choices = list("Met Station" = "met", "SedEvent" = "sed")),
      plotlyOutput("metPlotly",
                   height = "1000px"),
      br(),
      br(),
      plotlyOutput("sedPlotly",
                   height = "1000px")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ##### MET STATION DATA IMPORT  -----------------------------------------------------------------------------------------------

        # Read in pre-cleaned GOES Met Data
        metDataFileLocation <- "/srv/shiny-server/GOES_Data_Viewer_Shiny_App/data/goes_met_data.csv"
        metDatClean <- read_csv(metDataFileLocation, show_col_types = FALSE) %>%
          mutate(datetimePST = as.character(datetimePST))
          
        # Create table of most recent values.
        metDatRecent <- metDatClean %>% 
            group_by(stationID) %>%
            filter(datetimePST == max(datetimePST))
        
    ##### SEDEVENT DATA IMPORT - GOES --------------------------------------------------------------------------------
  
        # Read in pre-cleaned SedEvent Data
        sedDataFileLocation <- "/srv/shiny-server/GOES_Data_Viewer_Shiny_App/data/goes_sedevent_data.csv"
        sedDataClean <- read_csv(sedDataFileLocation, show_col_types = FALSE) %>%
          mutate(datetimeUTC = as.character(datetimeUTC))
        
        # Create table of most recent data.
        sedDataRecent <- sedDataClean %>% 
          group_by(stationID) %>%
          filter(datetimeUTC == max(datetimeUTC)) %>%
          distinct()
        
        
        ##### SEDEVENT DATA IMPORT  -  Iridium   ----------------------------------------------------------------------

        sedDataIridium <- read_csv("/srv/shiny-server/GOES_Data_Viewer_Shiny_App/data/iridium_sedevent_data.csv",
                                   show_col_types = FALSE) %>%
          mutate(datetimeUTC = as.character(datetimeUTC)) 
        
        
        sedDataIridiumRecent <- sedDataIridium %>% 
          group_by(stationID) %>%
          filter(datetimeUTC == max(datetimeUTC))
        
        
        ##### SEDEVENT DATA MERGE  -  GOES + Iridium   ----------------------------------------------------------------------    
        
        sedDataMerged <- bind_rows(sedDataClean, sedDataIridium) %>%
          dplyr::arrange(desc(datetimeUTC), stationID)
        sedDataRecentMerged <- bind_rows(sedDataRecent, sedDataIridiumRecent) %>%
          dplyr::arrange(desc(datetimeUTC), stationID)
        
        
    ##### OUTPUT Data Tables -------------------------------------------------------
        
        ## DT Tables
        output$metTable <- DT::renderDataTable({
          DT::datatable(metDatRecent, options = list(pageLength = 4)) 
          })
  
        output$sedEventTable <- DT::renderDataTable({
          DT::datatable(sedDataRecentMerged, options = list(pageLength = 13)) 
          }) 
        
        # GT Tables
        output$gtMetTable <- render_gt({
          metKey <- tribble(
          ~stationID, ~stationName,
          "SU", "Springdale Upper",
          "BU", "Blue Grouse Upper",
          "TL", "Trips Lower",
          "CL", "Coxit Lower"
        )
        
        metdat <- left_join(metDatRecent, metKey, by = "stationID")
        
        metdat %>%
          ungroup() %>%
          select(-'stationID') %>%
          gt(rowname_col = "stationName") %>%
          cols_align(align = "center") %>%
          cols_align(
            align = "right",
            columns = stationName) %>%
          opt_horizontal_padding(scale = 3) %>%
          tab_header(title = "Met Station")
        })
        output$gtSedTable <- render_gt({
          
          sedDataRecentMerged %>%
            group_by(basinPair) %>%
            gt(rowname_col = "stationID", groupname_col = "basinPair") %>%
            cols_align(align = "center") %>%
            opt_horizontal_padding(scale = 3) %>%
            tab_header(title = "Sed Event") %>%
            tab_style(
              style = cell_fill(color = "gray85"),
              locations = cells_row_groups(groups = everything()))
          
        })
        
          
    ##### OUTPUT Plots -------------------------------------------------------      
        
        # Both plots are within observe.  Not sure this is the cleanest way to do
        # this but it works.
        observe({
          
        plotCheckGroup <- input$plotCheckGroup
        
        ### Sed Event Plots --------------------------------------------------
        
        if ('sed' %in% plotCheckGroup) {
          output$sedPlotly <- renderPlotly({
            # Create ggplot
            sedPlot <- sedDataMerged %>%
              mutate(datetimeUTC = ymd_hms(datetimeUTC)) %>%
              pivot_longer(cols = -c(datetimeUTC, stationID, telem_source),
                           names_to = "variable", values_to = "value") %>%
              ggplot(aes(x = datetimeUTC, y = value, color = stationID)) +
              geom_line() +
              geom_point(size = 1) +
              theme_bw() +
              facet_grid(row = vars(variable),
                         scales = "free") +
              theme(legend.position = c(0.8, 0.2)) +
              labs(title = "Sed Event", y = "")

            # Make plotly from above ggplot
            sedPlot <- ggplotly(sedPlot,
                                width = 1400,
                                height = 1000,
                                dynamicTicks = TRUE) %>%
              layout(legend = list(orientation = "h", x = 0.2, y = 1.06,
                                   font = list(size = 24)),
                     title = list(font = list(size = 24))) %>%
              toWebGL()

            sedPlot
          }) } else {
            output$sedPlotly <- renderPlotly({})}
          
        ### Met Station Plot ---------------------------------------------------
        
          if ('met' %in% plotCheckGroup) {
            output$metPlotly <- renderPlotly({
              # Create ggplot
              metPlot <- metDatClean %>%
                mutate(datetimePST = ymd_hms(datetimePST)) %>%
                pivot_longer(cols = -c(datetimePST, stationID), names_to = "variable", values_to = "value") %>%
                ggplot(aes(x = datetimePST, y = value, color = stationID)) +
                geom_line() +
                geom_point(size = 1) +
                theme_bw() +
                facet_grid(row = vars(variable),
                           scales = "free") +
                labs(title = "Met Stations", y = "")
              
              metPlot <- ggplotly(metPlot,
                                  width = 1400,
                                  height = 1000,
                                  dynamicTicks = TRUE) %>%
                layout(legend = list(orientation = "h", x = 0.5, y = 1.06,
                                     font = list(size = 24)),
                       title = list(font = list(size = 24))) %>%
                toWebGL()
              
              metPlot
            }) } else {
              output$metPlotly <- renderPlotly({})}
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
