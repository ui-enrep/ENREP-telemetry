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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ENREP GOES Data Viewer"),

sidebarLayout(
    
    sidebarPanel(width = 0),
    
    mainPanel(
        DT::dataTableOutput("metTable"),
        DT::dataTableOutput("sedEventTable")
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
         
        # Read in data from ownCloud
        metDatRaw <- scan("https://www.northwestknowledge.net/cloud/index.php/s/rA5VkxMqzSmsFxT/download",
                       what = "character")
        
        # a "key" to tie the NESID value to our naming convention
        siteKey <- c(".*EE305504.*" = "BU",
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
            mutate(stationID = str_replace_all(string = stationID, pattern = siteKey),
                   voltage_V = as.numeric(voltage_V),
                   airTemp_C = as.numeric(airTemp_C), 
                   snowDepth_m = as.numeric(snowDepth_m),
                   accumPrecip_mm = as.numeric(accumPrecip_mm)) %>%
            arrange(stationID, datetimePST)
        
        # Create table of most recent values (might be more efficient/cleaner to have this called elsewhere?)
        metDatRecent <- metDatClean %>% 
            group_by(stationID) %>%
            filter(datetimePST == max(datetimePST))
    
    ##### SEDEVENT DATA IMPORT --------------------------------------------------------------------------------
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
        
        sedDataRaw <- scan("https://www.northwestknowledge.net/cloud/index.php/s/Ahzqw7G1s1riFVG/download", what = "character")
        sedDataClean <- sedDataRaw %>% 
            matrix(., ncol = 4, byrow = TRUE) %>% 
            data.frame() %>%
            separate(X1, into = c("nesid", "yy", "day", "time"), sep = c(8,10,13,19)) %>%
            separate(X4, c("voltage_V", "h2Temp_C", "stage_ft", "LSU"), sep = ",") %>%
            mutate(yy = paste("20", yy, sep = "")) %>%
            mutate(date = as_date(as.numeric(day), origin = ymd(paste0(yy, "-01-01", sep = "")))) %>%
            unite("datetimeUTC", c("date", "X3"), remove = TRUE, sep = " " ) %>%
            mutate(stationID = str_replace_all(string = nesid, pattern = sedSiteKey)) %>%
            select(stationID, datetimeUTC, voltage_V, h2Temp_C, stage_ft, LSU) %>%
            arrange(stationID, datetimeUTC)
        
        sedDataRecent <- sedDataClean %>% 
            group_by(stationID) %>%
            filter(datetimeUTC == max(datetimeUTC))
        
    ##### OUTPUT Data Tables -------------------------------------------------------
        output$metTable <- DT::renderDataTable({
            DT::datatable(metDatRecent)
        })
        output$sedEventTable <- DT::renderDataTable({
            DT::datatable(sedDataRecent)
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
