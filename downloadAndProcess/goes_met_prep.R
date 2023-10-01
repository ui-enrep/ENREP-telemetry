library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(here)
library(readr)

metDataRaw <- system("/home/ihellman/enrep/LrgsClient/bin/getDcpMessages -h lrgseddn1.cr.usgs.gov -u enrep1 -f /home/ihellman/enrep/LrgsClient/MessageBrowser_Met.sc -P yrqy-XWZB-96",
                    intern = TRUE)

# a "key" to tie the NESID value to our naming convention
metsiteKey <- c(".*EE305504.*" = "BU",
                ".*EE305BD6.*" = "SU",
                ".*EE30609E.*" = "TL",
                ".*EE306E4C.*" = "CL"
)

# File read in is one long character string.  Here, we split it into separate 
# elements and remove some junk data
metData <- metDataRaw %>%
  str_flatten() %>%
  str_split(pattern = "(?=EE)") %>%                             #  split at GOES ID.
  unlist() %>%                                                  # str_split returns list, this flattens it.
  str_squish() %>%                                              # remove white space (and /n type characters)
  str_remove('"|/') %>%                                         # Remove various characters and junk data
  str_subset(pattern = "\\$", negate = T) %>%
  str_remove_all(pattern = "\\,") %>%
  str_replace_all(pattern = "//////", replacement = "") %>%
  stringi::stri_remove_empty()

# Turn to dataframe and separate string into columns.
metData <- data.frame(dat = metData)
metData <- metData %>% separate_wider_delim(dat, delim = " ", 
                                            names = c("stationID", 
                                                      "date", 
                                                      "time", 
                                                      "voltage_V", 
                                                      "airTemp_C", 
                                                      "snowDepth_m", 
                                                      "accumPrecip_mm"), too_few = "align_start") %>% 
  drop_na() %>%    # helps alleviate problems with some junk data that comes in
  distinct()       # sometimes the same values are pulled in, here we delete duplicates.

# Final cleaning.  set data types and organize data frame.
metDataClean <- metData %>%
  mutate(stationID = str_replace_all(string = stationID, pattern = metsiteKey),
         date = as.character(ymd(date)),
         datetimePST = str_c(date, time, sep = " "), 
         voltage_V = as.numeric(voltage_V),
         airTemp_C = as.numeric(airTemp_C), 
         snowDepth_m = as.numeric(snowDepth_m),
         accumPrecip_mm = as.numeric(accumPrecip_mm)) %>%
  select(stationID, datetimePST, voltage_V, airTemp_C, snowDepth_m, accumPrecip_mm) %>%
  arrange(desc(datetimePST), stationID)

print(metDataClean)


# Location of stored data (either existing or to be saved)
#dataFileLocation <- "/srv/shiny-server/sample-apps/GOES_Data_Viewer_Shiny_App/data/goes_met_data.csv"
dataFileLocation <- here("data/goes_met_data.csv")

# Logic where: if data file does not exist, create one.  If it does exist then import it and
# merge with newly downloaded data.
if (file.exists(dataFileLocation)){
  # read In existing data
  existing_data <- read_csv(dataFileLocation, show_col_types = FALSE) %>%
    mutate(datetimePST = as.character(datetimePST))
  
  # new_data and existing data somehow have different datatypes which seems impossible
  # given they are in same table.  hack fix is as.character below.  very weird.
  merged_data <- bind_rows(existing_data, metDataClean) %>% 
    mutate(datetimePST = as.character(datetimePST)) %>%
    distinct()
  
  write.csv(merged_data, dataFileLocation, row.names = FALSE)
  
} else {
  write.csv(metDataClean, dataFileLocation, row.names = FALSE)
}
