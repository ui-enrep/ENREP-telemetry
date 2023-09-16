# Ian Hellman
# 2022-02-03
#
# This script extracts Iridium telemetry data from the FTS360 website by  navigating
# to each basin's page and copying the table of current values.

library(tidyverse)
library(RSelenium)
library(wdman)

# List of iridium pages on FTS360.  Used to iterate,
stationdf <- tribble(
  ~basin, ~siteurl,
  "bn_iridium", "https://360.ftsinc.com/218/station/6959",
  "fw_iridium", "https://360.ftsinc.com/218/station/6688",
  "te_iridium", "https://360.ftsinc.com/218/station/6650",
  "tw_iridium", "https://360.ftsinc.com/218/station/6960"
  
) %>% data.frame()

# start web browser session.  Set to be "headless" (i.e. hidden window).  Remove the
# extraCababilities from rsDriver to actually see the browser.
eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

# The chromever is a giant pain and needs to match your computer's version.  
# It sounds like there is a way to have it point to a copy that doesnt change.
# Also, if you removed the extraCapabilities argument, you can watch the browser 
# as it scrapes away.
rD <- rsDriver(browser = "chrome", chromever = "97.0.4692.71", extraCapabilities = eCaps)
remDr <- rD[["client"]]

# some VERY secure entry of usernames
username <- "uienrep@gmail.com"
pass <- "ftsenrep"

# go to page, enter login and pw, and click login
remDr$open()
remDr$navigate("https://360.ftsinc.com/login")
Sys.sleep(5)

remDr$findElement(using = "id", value = "email")$sendKeysToElement(list(username))
remDr$findElement(using = "id", value = "password")$sendKeysToElement(list(pass))
remDr$findElements("id", "login-button")[[1]]$clickElement()
Sys.sleep(5)

# Navigate to each site's url, scrape data from latest data table, and clean the data
scrapefts <- function(basin, siteurl){
  
  #navgiate to page.
  remDr$navigate(siteurl)
  Sys.sleep(5)
  
  # find table and download data to readable format
  tableElem <- remDr$findElement(using = "class", "grid-page")
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
    rename("voltage_V" = "X1", "h2temp_C" = "X2", "stage_ft" = "X3", "LSU" = "X4") %>%
    add_column(dateTime = dateTimeExtracted, .before = TRUE,
               basin = basin)
  print(dat)
  return(dat)
}

# scrape data for all stations listed in stationdf dataframe.
dat <- pmap_df(stationdf, scrapefts)

# close browser sesssions.
remDr$close()
rD$server$stop()
