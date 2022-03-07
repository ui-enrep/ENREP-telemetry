#  ENREP GOES Data Viewer
#  Ian Hellman
#  12-Oct-2021
#
#  This app pulls, cleans, and displays raw GOES telemetry data.  The data used currently is simply pulled from
#  the project's ownCloud shared drive (ENREP_Shared).  The actually downloading of raw goes data is done on
#  a linux machine which retrieves GOES data from NOAA every 12 hours via the LRGS command line tools.  From
#  there it is sync to teh ENREP_Shared folder.  
#
#  Future goals:
#  1) Have this app do all of the work using the Java LRGS command line tools.  Not sure if this is even possible
#     if you are hosting the app on shinyapps.io
#  2) Currently, the app can only reach out and get a single shared text file that is being appended to on ownCloud.
#     If you try to download a folder, it gets a .tar file.  This is manageable on a local shiny server but for 
#     some reason (file permissions I think) it does not work on shinyapps.io.  The code where this worked locally is
#     Commented out in the import section below.
#  3) Incorporate the Iridium telemetry.  That would allow this platform to be better in many ways than FTS 360.
#

library(shiny)
library(tidyverse)
library(lubridate)
library(glue)
library(here)
library(DT)
library(curl)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ENREP GOES Data Viewer"),

sidebarLayout(
    # remove sidebar
    sidebarPanel(width = 0),
    
    mainPanel(
        DT::dataTableOutput("metTable"),
        DT::dataTableOutput("sedEventTable"),
        br(),
        br(),
        plotlyOutput("metPlotly",
                     height = "1000px",
                     width = "1000px"),
        br(),
        br(),
        plotlyOutput("sedPlotly",
                     height = "1000px",
                     width = "1000px")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ##### MET STATION DATA IMPORT  -------------------------------------------------------------------------------------------------
        ### An attempt at getting multiple files that automatically tarred by ownCloud.  Works locally but not on Shinapps.io.
        ### Keeping here in case of a deployment with more file permissions.
            # tempTAR <- tempfile(fileext = ".tar")
            # curl_download("https://www.northwestknowledge.net/cloud/index.php/s/4l8GhFgrBa0aeOW/download", destfile = tempTAR)
            # untar(normalizePath(tempTAR), exdir = normalizePath(tempdir()))
            # metDatRaw <- scan(paste(normalizePath(tempdir()), "/GOES_Telemetry/GOES_raw.txt", sep = ""),
            #                what = "character")
         
        # Read in data from ownCloud (note: this link is from copying the link from the download button when following the shared link)
        metDatRaw <- scan("https://www.northwestknowledge.net/cloud/index.php/s/rA5VkxMqzSmsFxT/download",
                       what = "character")
        
        # a "key" to tie the NESID value to our naming convention
        metsiteKey <- c(".*EE305504.*" = "BU",
                     ".*EE305BD6.*" = "SU", 
                     ".*EE30609E.*" = "TL", 
                     ".*EE306E4C.*" = "CL"
                     )
        
        # Clean up raw data (a vector of strings) to a usable dataframe.  
        metDatClean <- str_replace(metDatRaw, pattern = c(","), replacement = "") %>%         # Remove "," from data
            matrix(., ncol = 7, byrow = TRUE) %>%
            data.frame() %>%
            distinct() %>%
            rename(stationID = X1,
                   date = X2,
                   time = X3,
                   voltage_V = X4,
                   airTemp_C = X5,
                   snowDepth_m = X6,
                   accumPrecip_mm = X7) %>%
            unite("datetimePST", c("date", "time"), remove = TRUE, sep = " ") %>%
            mutate(stationID = str_replace_all(string = stationID, pattern = metsiteKey),
                   voltage_V = as.numeric(voltage_V),
                   airTemp_C = as.numeric(airTemp_C), 
                   snowDepth_m = as.numeric(snowDepth_m),
                   accumPrecip_mm = as.numeric(accumPrecip_mm)) %>%
            arrange(stationID, datetimePST)
        
        # Create table of most recent values (might be more efficient/cleaner to have this called elsewhere?)
        metDatRecent <- metDatClean %>% 
            group_by(stationID) %>%
            filter(datetimePST == max(datetimePST))
    
    ##### SEDEVENT DATA IMPORT  -  GOES   --------------------------------------------------------------------------
  
        # Read in data from ownCloud.  This is v2 method of reading and cleaning data.  Could change met station
        # method to this as well.
        sedDataRaw <- read_file("https://www.northwestknowledge.net/cloud/index.php/s/Ahzqw7G1s1riFVG/download")
        
        # a "key" to tie the NESID value to our naming convention
        sedSiteKey <- c(
          ".*EE300578.*" = "SS" ,
          ".*EE300BAA.*" = "SN" ,
          ".*EE30160E.*" = "BN" ,
          ".*EE3018DC.*" = "BS" ,
          ".*EE302394.*" = "TE" ,
          ".*EE302D46.*" = "TW" ,
          ".*EE3030E2.*" = "FW" ,
          ".*EE303E30.*" = "FE" ,
          ".*EE304672.*" = "CW" ,
          ".*EE3048A0.*" = "CE"
        )
        
        # Convert single sedDataRaw string to slightly cleaner vector of strings 
        sedDataCleanVector <- sedDataRaw %>%
          str_split(pattern = "(?=EE)") %>% 
          unlist() %>% 
          str_squish() %>%
          str_replace('"|/', "")
        
        # Mega cleaning to final table. Arguably too much in one run.
        sedDataClean <- tibble(raw = sedDataCleanVector) %>%
          filter(raw != "") %>% 
          separate(raw, into = c("id_and_date", "data"), sep = c(37)) %>%
          separate(id_and_date, into = c("nesid", "yy", "day", NA), sep = c(8,10,13,19), remove = TRUE) %>%
          separate(data, c(NA, NA, "datetimeUTC", "dataVals"), sep = " ", remove = TRUE) %>%
          separate(dataVals, c("voltage_V", "h2Temp_C", "stage_ft", "LSU"), sep = ',', remove = TRUE) %>%
          mutate(yy = paste("20", yy, sep = "")) %>%
          # The "- 1" below is because GOES's says that YYYY-01-01 is Julain day 0 whereas R says its 1.  Wow.
          mutate(date = as_date(as.numeric(day) - 1, origin = ymd(paste0(yy, "-01-01", sep = "")))) %>%
          unite("datetimeUTC", c("date", "datetimeUTC"), remove = TRUE, sep = " " ) %>%
          mutate(stationID = str_replace_all(string = nesid, pattern = sedSiteKey)) %>%
          select(stationID, datetimeUTC, voltage_V, h2Temp_C, stage_ft, LSU) %>%
          mutate(voltage_V = as.numeric(voltage_V),
                 h2Temp_C = as.numeric(h2Temp_C),
                 stage_ft = as.numeric(stage_ft),
                 LSU = as.numeric(LSU),
                 telem_source = "GOES") %>%
          dplyr::arrange(stationID, datetimeUTC) %>%
          drop_na()
        
        # Create table of most recent data.  Cleans up table making (maybe?)
        sedDataRecent <- sedDataClean %>% 
            group_by(stationID) %>%
            filter(datetimeUTC == max(datetimeUTC))
        
    ##### SEDEVENT DATA IMPORT  -  Iridium   ----------------------------------------------------------------------
        #sedDataIridium <- read_file("NKNLINK")
        sedDataIridium <- read_csv("https://www.northwestknowledge.net/cloud/index.php/s/rDpwrGcGlMZOhqm/download") %>%
          mutate(datetimeUTC = as.character(datetimeUTC)) 
          
        
        sedDataIridiumRecent <- sedDataIridium %>% 
          group_by(stationID) %>%
          filter(datetimeUTC == max(datetimeUTC))
        
    ##### SEDEVENT DATA MERGE  -  GOES + Iridium   ----------------------------------------------------------------------    
        sedDataRecentMerged <- bind_rows(sedDataRecent, sedDataIridiumRecent)
        
    ##### OUTPUT Data Tables -------------------------------------------------------
        
        output$metTable <- DT::renderDataTable({
            DT::datatable(metDatRecent)
        })
        
        output$sedEventTable <- DT::renderDataTable({
            DT::datatable(sedDataRecentMerged, options = list(pageLength = 25))
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
          
          # Make plotly from above ggplot
          ggplotly(metPlot, 
                   width = 1000, 
                   height = 1000,
                   dynamicTicks = TRUE) %>%
          layout(legend = list(orientation = "h", x = 0.5, y = 1.06))
          
        })
    
        #SedEvent plotly output
        output$sedPlotly <- renderPlotly({
          # Create ggplot
          sedPlot <- sedDataClean %>% 
            mutate(datetimeUTC = ymd_hms(datetimeUTC)) %>%
            pivot_longer(cols = -c(datetimeUTC, stationID, telem_source), names_to = "variable", values_to = "value") %>%
            ggplot(aes(x = datetimeUTC, y = value, color = stationID)) +
            geom_line() +
            geom_point(size = 1) +
            theme_bw() +
            facet_grid(row = vars(variable),
                       scales = "free") +
            theme(legend.position = c(0.8, 0.2)) +
            labs(title = "Sed Event", y = "") 
          
          # Make plotly from above ggplot
          ggplotly(sedPlot, 
                   width =1200, 
                   height = 1000,
                   dynamicTicks = TRUE) %>% 
          layout(legend = list(orientation = "h", x = 0.5, y = 1.06))
        })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
