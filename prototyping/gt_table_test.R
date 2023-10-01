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

dat %>%
  group_by(basinPair) %>%
  gt(rowname_col = "stationID", groupname_col = "basinPair") %>%
  cols_align(align = "center") %>%
  tab_header(title = "Sed Event") %>%
  tab_style(
    style = cell_fill(color = "gray85"),
    locations = cells_row_groups(groups = everything()))

    