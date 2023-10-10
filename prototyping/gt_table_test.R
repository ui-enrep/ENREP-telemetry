library(gt)

key <- tribble(
  ~stationID, ~basinPair,
  "BN", "Blue Grouse",
  "BS", "Blue Grouse",
  "SN", "Springdale",
  "SS", "Springdale",
  "TW", "Tripps Knob",
  "TE", "Tripps Knob",
  "FE", "Fish Creek",
  "FW", "Fish Creek",
  "CE", "Coxit Creek",
  "CW", "Coxit Creek"
)

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

dat <- left_join(sedDataRecentMerged, key, by = "stationID")

dat %>%
  group_by(basinPair) %>%
  gt(rowname_col = "stationID", groupname_col = "basinPair") %>%
  cols_align(align = "center") %>%
  opt_horizontal_padding(scale = 3) %>%
  tab_header(title = "Sed Event") %>%
  tab_style(
    style = cell_fill(color = "gray85"),
    locations = cells_row_groups(groups = everything()))


metKey <- tribble(
  ~stationID, ~stationName,
  "SU", "Springdale Upper",
  "BU", "Blue Grouse Upper",
  "TL", "Trips Lower",
  "CL", "Coxit Lower"
)

##### MET STATION DATA IMPORT  -----------------------------------------------------------------------------------------------

# Read in pre-cleaned GOES Met Data
metDataFileLocation <- "/srv/shiny-server/GOES_Data_Viewer_Shiny_App/data/goes_met_data.csv"
metDatClean <- read_csv(metDataFileLocation, show_col_types = FALSE) %>%
  mutate(datetimePST = as.character(datetimePST))

# Create table of most recent values.
metDatRecent <- metDatClean %>% 
  group_by(stationID) %>%
  filter(datetimePST == max(datetimePST))

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
