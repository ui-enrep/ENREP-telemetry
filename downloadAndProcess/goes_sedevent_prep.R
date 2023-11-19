library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(tidyr)
library(lubridate)

# Read in data from ownCloud

## Reading in raw data and filtering the NULL character fixes issue

sedDataRaw <- system("/home/ihellman/enrep/LrgsClient/bin/getDcpMessages -h lrgseddn1.cr.usgs.gov -u enrep1 -f /home/ihellman/enrep/LrgsClient/MessageBrowser_SedEvent.sc -P yrqy-XWZB-96",
                     intern = TRUE)

# Key to relate nesid to ENREP naming conventions
sedSiteKey <- tribble(
  ~nesid,  ~stationID, ~basinPair, 
  "EE300578", "SS" , "Springdale" , 
  "EE300BAA", "SN" , "Springdale" , 
  "EE30160E", "BN" , "Blue Grouse" ,
  "EE3018DC", "BS" , "Blue Grouse" ,
  "EE302394", "TE" , "Tripps Knob" ,
  "EE302D46", "TW" , "Tripps Knob" ,
  "EE3030E2", "FW" , "Fish Creek" ,
  "EE303E30", "FE" , "Fish Creek" ,
  "EE304672", "CW" , "Coxit" ,
  "EE3048A0", "CE" , "Coxit"
)

# Convert single sedDataRaw string to slightly cleaner vector of strings 
sedDataCleanVector <- sedDataRaw %>%
  str_flatten() %>%
  str_split(pattern = "(?=EE)") %>% 
  unlist() %>% 
  str_squish() %>%
  str_remove('"|/') %>%
  stringi::stri_remove_empty()

# Main cleaning
sedDataClean  <- tibble(raw = sedDataCleanVector) %>%
  separate(raw, into = c("id_and_date", "data"), sep = " 0 ") %>%
  filter(data != "0") %>%
  separate(id_and_date, into = c("nesid", "yy", "day", NA), sep = c(8,10,13,19), remove = TRUE) %>%
  separate(data, c("datetimeUTC", "dataVals"), sep = " ", remove = TRUE) %>%
  separate(dataVals, c("voltage_V", "h2Temp_C", "stage_ft", "LSU"), sep = ',', remove = TRUE) %>%
  mutate(yy = paste("20", yy, sep = "")) %>%
  # -1 from date because R treats origin as Julian day 0 
  # while sed station treats it as Julian day 1
  mutate(date = as_date(as.numeric(day) - 1, origin = ymd(paste0(yy, "-01-01", sep = "")))) %>%
  unite("datetimeUTC", c("date", "datetimeUTC"), remove = TRUE, sep = " " )

# Add field for basinPair and stationID and some more cleaning.  
sedDataClean <- sedDataClean %>%
  left_join(sedSiteKey, by = "nesid") %>%
  select(stationID, datetimeUTC, voltage_V, h2Temp_C, stage_ft, LSU, basinPair) %>%
  mutate(voltage_V = as.numeric(voltage_V),
         h2Temp_C = as.numeric(h2Temp_C),
         stage_ft = as.numeric(stage_ft),
         LSU = as.numeric(LSU),
         telem_source = "GOES") %>%
  drop_na() 

# Location of stored data (either existing or to be saved)
dataFileLocation <- "/srv/shiny-server/ENREP-telemetry/data/goes_sedevent_data.csv"

# Logic where: if data file does not exist, create one.  If it does exist then import it and
# merge with newly downloaded data.
if (file.exists(dataFileLocation)){
  # read In existing data
  existing_data <- read_csv(dataFileLocation, show_col_types = FALSE) %>%
    mutate(datetimeUTC = as.character(datetimeUTC))
  
  # new_data and existing data somehow have different datatypes which seems impossible
  # given they are in same table.  hack fix is as.character below.  very weird.
  merged_data <- bind_rows(existing_data, sedDataClean) %>% 
    mutate(datetimeUTC = as.character(datetimeUTC)) %>%
    distinct() %>%
    arrange(datetimeUTC)
  
  write.csv(merged_data, dataFileLocation, row.names = FALSE)
  
} else {
  write.csv(sedDataClean, dataFileLocation, row.names = FALSE)
}
