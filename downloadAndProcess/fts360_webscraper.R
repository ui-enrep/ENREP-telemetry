# Ian Hellman
# 2022-02-03
#
# This script extracts Iridium telemetry data from the FTS360 website by  navigating
# to each basin's page and copying the table of current values.

library(tidyverse)
library(RSelenium)
library(wdman)
library(here)

# Start a docker container with chrome. 

# Check if the selenium/standalone-chrome docker image named "fts_scrape" is running. This script 
# was having some browser error issues and if it doesnt finish, the container will be left 
# running and cause it to not run again.
# 
# The container selenium/standalone-chrome:111.0 specified is the latest version that works. The latest version
# was 114 at time of writing  but the remDr$open() function just times out when using the latest.

fts_scrape_running <- system("docker ps", intern = TRUE) %>% str_detect("fts_scrape") %>% any()

# If fts_scrape container is running, kill it.  If not, start it.
if (fts_scrape_running == TRUE){
  #system("docker kill fts_scrape")
  #system("docker rm fts_scrape") # remove docker.  This prevents lots of dead docker instances from piling up.
  #system("docker run --name fts_scrape -d -p 4445:4444 --shm-size=2g selenium/standalone-chrome:111.0")
  print("Selenium docker already running")
} else {
  system("docker run --name fts_scrape -d -p 4445:4444 --shm-size=2g selenium/standalone-chrome:111.0")
}

Sys.sleep(5)


### Actual scraping -----------------------------------------------------------------------------------------------
#Connect to server -> Find Iridium data table -> repeat for all sites -> merge to one table

# List of iridium pages on FTS360.  Used to iterate,
stationdf <- tribble(
  ~stationID, ~siteurl,
  "BN", "https://360.ftsinc.com/218/station/6160c6ba64699463087282b2",
  "FW", "https://360.ftsinc.com/218/station/6160c6b964699463087281dd",
  "TE", "https://360.ftsinc.com/218/station/6160c6b964699463087281bc",
  "TW", "https://360.ftsinc.com/218/station/6160c6ba64699463087282b5") %>% 
  data.frame()

# Create connection to chrome running in docker container.
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")

# some VERY secure entry of usernames
username <- "uienrep@gmail.com"
pass <- "ftsenrep"

# Go to login page
remDr$open()
Sys.sleep(5)
remDr$navigate("https://360.ftsinc.com/login")
Sys.sleep(10)

# Enter login info and press login button
# Email
remDr$findElement(using = "xpath", value = "//*[(@id = 'email')]")$sendKeysToElement(list(username))
# Password
remDr$findElement(using = "xpath", value = '//*[(@id = "password")]')$sendKeysToElement(list(pass))
# Login button
remDr$findElements("xpath", '//*[(@id = "login-button")]')[[1]]$clickElement()
Sys.sleep(5)

# Navigate to each site's url, scrape data from latest data table, and clean the data
scrapefts <- function(stationID, siteurl){
  
  #navgiate to page.
  remDr$navigate(siteurl)
  Sys.sleep(10)
  
  # find table and download data to readable format
  tableElem <- remDr$findElement(using = "xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "grid-page", " " ))]')
  tableElem$getElementAttribute("class")
  tableVec <- tableElem$getElementText()[[1]]
  
  #Extract date and clean it up a bit.
  dateTimeExtracted <- str_split(tableVec, pattern = "\n") %>% 
    unlist() %>% 
    str_subset(",") %>% 
    pluck(1) %>% 
    str_remove(pattern = " \\s*\\([^\\)]+\\)")
  
  # Clean and convert vector to data frame.
  dat <- str_split(tableVec, pattern = "\n") %>% 
    unlist() %>% 
    matrix(ncol = 3, byrow = TRUE) %>% 
    .[-1,2] %>%
    parse_number() %>%
    t() %>%
    data.frame() %>% 
    rename("voltage_V" = "X1", "h2Temp_C" = "X2", "stage_ft" = "X3", "LSU" = "X4") %>%
    add_column(datetimeUTC = dateTimeExtracted, .before = TRUE,
               stationID = stationID,
               telem_source = "Iridium") %>%
    mutate(datetimeUTC = lubridate::mdy_hms(datetimeUTC)) %>%
    mutate(datetimeUTC = as.character(datetimeUTC)) %>%
    relocate(stationID, .before = datetimeUTC)
  print(dat)
  return(dat)
}

# scrape data for all stations listed in station dataframe.
new_data <- pmap_df(stationdf, scrapefts)


###  Merge with existing data or export new --------------------------------------------------------------------

# Location of stored data (either existing or to be saved)
dataFileLocation <- here("data/iridium_sedevent_data.csv")

# Logic where: if data file does not exist, create one.  If it does exist then import it and
# merge with newly downloaded data.
if (file.exists(dataFileLocation)){
  # read In existing data
  existing_data <- read_csv(dataFileLocation, show_col_types = FALSE) %>%
    mutate(datetimeUTC = as.character(datetimeUTC))
  
  # new_data and existing data somehow have different datatypes which seems impossible
  # given they are in same table.  hack fix is as.character below.  very weird.
  merged_data <- bind_rows(existing_data, new_data) %>% 
    mutate(datetimeUTC = as.character(datetimeUTC),
           stage_ft = as.character(stage_ft)) %>%
    distinct()
  
  write.csv(merged_data, dataFileLocation, row.names = FALSE)
  
} else {
  write.csv(new_data, dataFileLocation, row.names = FALSE)
}


### Clean up --------------------------------------------------------------------------------------------------

# close selenium browser
remDr$close()

# kill docker container process
#system("docker kill fts_scrape")
#system("docker rm fts_scrape")
