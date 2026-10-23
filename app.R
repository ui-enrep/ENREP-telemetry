#  ENREP Telemetry Viewer (v2)
#  bslib + native plotly. Data is shared across sessions and only re-read
#  when the cron job rewrites the CSVs.

library(shiny)
library(bslib)
library(data.table)
library(plotly)
library(gt)

##### CONFIG -------------------------------------------------------------------

# Use the server data folder when it exists, otherwise the repo's data folder.
dataDir <- "/srv/shiny-server/ENREP-telemetry/data"
if (!dir.exists(dataDir)) dataDir <- "data"

dataFiles <- c(
  met     = file.path(dataDir, "goes_met_data.csv"),
  sedGoes = file.path(dataDir, "goes_sedevent_data.csv"),
  sedIrid = file.path(dataDir, "iridium_sedevent_data.csv")
)

metKey <- c(
  SU = "Springdale Upper",
  BU = "Blue Grouse Upper",
  TL = "Trips Lower",
  CL = "Coxit Lower"
)

metVars <- c(voltage_V = "Voltage (V)", airTemp_C = "Air Temp (C)",
             snowDepth_m = "Snow Depth (m)", accumPrecip_mm = "Accum. Precip (mm)")
sedVars <- c(voltage_V = "Voltage (V)", h2Temp_C = "Water Temp (C)",
             stage_ft = "Stage (ft)", LSU = "LSU")

timeframes <- c("1 Month" = 30, "3 Months" = 91, "6 Months" = 182,
                "1 Year" = 365, "All" = 0)

##### DATA ---------------------------------------------------------------------

# Cache keyed on file modification times, shared by all sessions.
dataCache <- new.env()

# Explicit format so a midnight value written as a bare date ("2026-10-19")
# can't make R fall back to date-only parsing for the whole column.
parseTime <- function(x, tz) {
  t <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = tz)
  miss <- is.na(t)
  t[miss] <- as.POSIXct(x[miss], format = "%Y-%m-%d", tz = tz)
  t
}

readData <- function() {
  met <- fread(dataFiles[["met"]], colClasses = list(character = "datetimePST"))
  met[, time := parseTime(datetimePST, "Etc/GMT+8")]   # PST, no DST
  met[, stationName := fifelse(stationID %in% names(metKey),
                               metKey[stationID], stationID)]

  sedCols <- c("stationID", "datetimeUTC", names(sedVars), "telem_source", "basinPair")
  readSed <- function(f) {
    # Read values as text so flags like "#2" show in the table as-is.
    d <- fread(f, colClasses = list(character = c("datetimeUTC", names(sedVars))),
               na.strings = c("", "NA"))
    d[, ..sedCols]
  }
  sed <- rbind(readSed(dataFiles[["sedGoes"]]), readSed(dataFiles[["sedIrid"]]))
  sed[, time := parseTime(datetimeUTC, "UTC")]

  # Rows without a usable time can't be plotted or ranked as "latest".
  met <- met[!is.na(time)]
  sed <- sed[!is.na(time)]
  setorder(met, -time)
  setorder(sed, -time)
  list(met = met, sed = sed)
}

loadData <- function() {
  version <- paste(file.mtime(dataFiles), collapse = "|")
  if (identical(dataCache$version, version)) return(dataCache$data)

  # If a CSV is mid-write by cron, keep serving the last good copy.
  fresh <- tryCatch(readData(), error = function(e) {
    if (is.null(dataCache$data)) stop(e)
    warning("Data reload failed, using cached data: ", conditionMessage(e))
    NULL
  })
  if (is.null(fresh)) return(dataCache$data)
  met <- fresh$met
  sed <- fresh$sed

  dataCache$data <- list(
    met       = met,
    sed       = sed,
    metLatest = unique(met, by = "stationID"),
    sedLatest = unique(sed, by = c("stationID", "telem_source")),
    version   = version
  )
  dataCache$version <- version
  dataCache$data
}

##### HELPERS ------------------------------------------------------------------

# Fixed color per station so it matches across every plot row.
stationColors <- function(stations) {
  stations <- sort(unique(stations))
  setNames(hcl.colors(length(stations), "Dark 3"), stations)
}

filterTime <- function(d, days) {
  if (days == 0) return(d)
  d[time >= Sys.time() - days * 86400]
}

# One row per variable, shared x-axis, one legend entry per station.
tsPlot <- function(d, vars, colors, tzLabel) {
  d <- d[order(time)]
  stations <- intersect(names(colors), unique(d$stationID))
  x <- format(d$time, "%Y-%m-%d %H:%M:%S")

  rows <- lapply(seq_along(vars), function(i) {
    v <- names(vars)[i]
    p <- plot_ly()
    for (s in stations) {
      idx <- d$stationID == s
      # Non-numeric flags (e.g. "#2") become gaps.
      y <- suppressWarnings(as.numeric(d[[v]][idx]))
      p <- add_trace(p, x = x[idx], y = y,
                     type = "scattergl", mode = "lines+markers",
                     name = s, legendgroup = s, showlegend = i == 1,
                     line = list(color = colors[[s]], width = 1.5),
                     marker = list(color = colors[[s]], size = 4),
                     hovertemplate = paste0("%{x}<br>", vars[[i]], ": %{y}<extra>", s, "</extra>"))
    }
    layout(p, xaxis = list(type = "date"),
           yaxis = list(title = list(text = vars[[i]], font = list(size = 12))))
  })

  subplot(rows, nrows = length(vars), shareX = TRUE, titleY = TRUE, margin = 0.02) |>
    layout(legend = list(orientation = "h", x = 0, y = 1.04),
           xaxis = list(type = "date", title = paste("Date/Time", tzLabel)),
           margin = list(t = 40)) |>
    config(displaylogo = FALSE)
}

latestTable <- function(gtObj, title) {
  gtObj |>
    cols_align(align = "center") |>
    opt_horizontal_padding(scale = 3) |>
    tab_header(title = title) |>
    tab_style(style = cell_fill(color = "gray92"),
              locations = cells_row_groups(groups = everything())) |>
    sub_missing() |>
    tab_options(table.width = pct(100))
}

##### UI -----------------------------------------------------------------------

# UI is a function so the pickers reflect the current data on each page load.
ui <- function(request) {
  dat <- loadData()
  basins <- sort(unique(dat$sed$basinPair))
  metStations <- dat$metLatest[order(stationName), setNames(stationID, stationName)]

  timeInput <- function(id) {
    radioButtons(id, "Time Frame", choices = timeframes, selected = 30)
  }

  page_navbar(
    title = "ENREP Telemetry",
    theme = bs_theme(version = 5),
    fillable = FALSE,

    nav_panel(
      "Sed Event",
      layout_sidebar(
        sidebar = sidebar(
          timeInput("sedTime"),
          checkboxGroupInput("sedBasins", "Basin Pair", choices = basins, selected = basins)
        ),
        card(uiOutput("sedTable")),
        card(card_header("Sed Event (UTC)"),
             plotlyOutput("sedPlot", height = "900px"))
      )
    ),

    nav_panel(
      "Met Station",
      layout_sidebar(
        sidebar = sidebar(
          timeInput("metTime"),
          checkboxGroupInput("metStations", "Station", choices = metStations,
                             selected = metStations)
        ),
        card(uiOutput("metTable")),
        card(card_header("Met Station (PST)"),
             plotlyOutput("metPlot", height = "900px"))
      )
    )
  )
}

##### SERVER -------------------------------------------------------------------

server <- function(input, output, session) {
  dat <- loadData()
  sedColors <- stationColors(dat$sed$stationID)
  metColors <- stationColors(dat$met$stationID)

  ### Latest value tables
  output$sedTable <- renderUI({
    # Basin pair groups in order of their latest report; within a group, rows
    # sorted by station so GOES/Iridium rows for the same site sit together.
    d <- dat$sedLatest
    d[order(match(basinPair, unique(basinPair)), stationID, -as.numeric(time)),
      c("stationID", "datetimeUTC", names(sedVars), "telem_source", "basinPair"),
      with = FALSE] |>
      gt(rowname_col = "stationID", groupname_col = "basinPair") |>
      cols_label(datetimeUTC = "Date/Time (UTC)", telem_source = "Telemetry",
                 .list = as.list(sedVars)) |>
      latestTable("Sed Event")
  })

  output$metTable <- renderUI({
    dat$metLatest[order(stationName),
                  c("stationName", "datetimePST", names(metVars)), with = FALSE] |>
      gt(rowname_col = "stationName") |>
      cols_label(datetimePST = "Date/Time (PST)", .list = as.list(metVars)) |>
      latestTable("Met Station")
  })

  ### Plots (cached across sessions until the data files change)
  output$sedPlot <- renderPlotly({
    d <- filterTime(dat$sed, as.numeric(input$sedTime))[basinPair %in% input$sedBasins]
    validate(need(nrow(d) > 0, "No Sed Event data for the selected time frame and basins."))
    tsPlot(d, sedVars, sedColors, "(UTC)")
  }) |>
    bindCache(dat$version, input$sedTime, sort(input$sedBasins))

  output$metPlot <- renderPlotly({
    d <- filterTime(dat$met, as.numeric(input$metTime))[stationID %in% input$metStations]
    validate(need(nrow(d) > 0, "No Met Station data for the selected time frame and stations."))
    tsPlot(d, metVars, metColors, "(PST)")
  }) |>
    bindCache(dat$version, input$metTime, sort(input$metStations))
}

shinyApp(ui = ui, server = server)
