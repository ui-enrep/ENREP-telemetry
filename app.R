#  ENREP GOES Data Viewer


library(readr)
library(dplyr)
library(tidyr)
library(shiny)
library(DT)
library(plotly)
library(ggplot2)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ENREP Telemetry"),

sidebarLayout(
    # remove sidebar
    sidebarPanel(width = 0),
    
    mainPanel(
      ## Check boxes added on 12/9/22 by Kaitlyn Strickfaden
      ## Allow user to choose if only most recent data are shown
      ## or all data for quality checks 
      checkboxInput("metCheck", "Show all met station data?", value = F),
      DT::dataTableOutput("metTable"),
      br(),
      br(),
      checkboxInput("sedCheck", "Show all sed event data?", value = F),
      DT::dataTableOutput("sedEventTable"),
      br(),
      br(),
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
        
        observe({
        
        metCheck <- input$metCheck
        sedCheck <- input$sedCheck
        
        if (metCheck == F) {
        output$metTable <- DT::renderDataTable({
            DT::datatable(metDatRecent, options = list(pageLength = 4))
        }) } else {
          output$metTable <- DT::renderDataTable({
            DT::datatable(metDatClean, options = list(pageLength = 4)) 
          }) }
        
        
        if (sedCheck == F) {
          output$sedEventTable <- DT::renderDataTable({
            DT::datatable(sedDataRecentMerged, options = list(pageLength = 13))
          }) } else {
            output$sedEventTable <- DT::renderDataTable({
              DT::datatable(sedDataMerged, options = list(pageLength = 13)) 
            }) }
        
        })
        
        # Met station plotly output
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
          
        })
    
        #SedEvent plotly output
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
        })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
