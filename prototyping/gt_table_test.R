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

met <- read_csv("/srv/shiny-server/GOES_Data_Viewer_Shiny_App/data/goes_sedevent_data.csv")

dat <- left_join(met, key, by = "stationID")

dat %>%
  group_by(basinPair) %>%
  gt(rowname_col = "stationID", groupname_col = "basinPair") %>%
  cols_align(align = "center") %>%
  tab_header(title = "Sed Event") %>%
  tab_style(
    style = cell_fill(color = "gray85"),
    locations = cells_row_groups(groups = everything()))

    