# Ian Hellman
# 2026-10-19
#
# This script pulls Iridium telemetry data from the FTS360 API (the same backend the
# 360.ftsinc.com website uses) and appends it to the stored CSV. Replaces the old
# Selenium/Docker scraper.
#
# Credentials are read from environment variables. Add these lines to ~/.Renviron
# on the server:
#   FTS_USER=...
#   FTS_PASS=...

library(dplyr)
library(readr)
library(tibble)
library(httr2)

print("Starting FTS360 API download")
print(paste0("System Time: ", Sys.time()))

api_base <- "https://fts360api.com"
agency_id <- 218

# How far back to request each run. Overlap with previous runs is deduplicated below,
# so a longer window just backfills any runs that were missed.
lookback_days <- 3

# Stations to download. stationID is our code; ftsID is the id in the FTS360 station URL.
stationdf <- tribble(
  ~stationID, ~ftsID, ~basinPair,
  "BN", "6160c6ba64699463087282b2", "Blue Grouse",
  "FW", "6160c6b964699463087281dd", "Fish Creek",
  "TE", "6160c6b964699463087281bc", "Tripps Knob",
  "TW", "6160c6ba64699463087282b5", "Tripps Knob")


### Log in -----------------------------------------------------------------------------------------------------

if (Sys.getenv("FTS_USER") == "" || Sys.getenv("FTS_PASS") == "") {
  stop("FTS_USER / FTS_PASS not set. Add them to ~/.Renviron (then restart R).")
}

auth <- request(api_base) |>
  req_url_path("/auth/v1/users/authenticate") |>
  req_body_json(list(email = Sys.getenv("FTS_USER"),
                     password = Sys.getenv("FTS_PASS"))) |>
  req_retry(max_tries = 3) |>
  req_perform() |>
  resp_body_json()

token <- auth$access_token


### Download records --------------------------------------------------------------------------------------------

start_date <- format(Sys.time() - lookback_days * 86400, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

csv_text <- request(api_base) |>
  req_url_path("/data/v1/agencies", agency_id, "records/csv") |>
  req_url_query(stationIds = stationdf$ftsID, startDate = start_date, .multi = "explode") |>
  req_auth_bearer_token(token) |>
  req_headers(Accept = "text/csv") |>
  req_retry(max_tries = 3) |>
  req_perform() |>
  resp_body_string()

# Read everything as character; Stage occasionally comes through as a flag like "#2".
new_data <- read_csv(I(csv_text), col_types = cols(.default = col_character())) |>
  transmute(ftsID = `Station ID`,
            datetimeUTC = format(lubridate::ymd_hms(Date), "%Y-%m-%d %H:%M:%S"),
            voltage_V = VB,
            h2Temp_C = YI,
            stage_ft = Stage,
            LSU = LSU,
            telem_source = "Iridium") |>
  inner_join(stationdf, by = "ftsID") |>
  select(stationID, datetimeUTC, voltage_V, h2Temp_C, stage_ft, LSU, telem_source, basinPair)

print(new_data, n = Inf)


###  Merge with existing data or export new --------------------------------------------------------------------

# Location of stored data (either existing or to be saved). Set FTS_DATA_FILE to
# override, e.g. when testing locally.
dataFileLocation <- Sys.getenv("FTS_DATA_FILE",
                               "/srv/shiny-server/ENREP-telemetry/data/iridium_sedevent_data.csv")

if (file.exists(dataFileLocation)){
  existing_data <- read_csv(dataFileLocation, col_types = cols(.default = col_character()))

  # Keep the first copy of each station/time so re-downloaded records aren't duplicated.
  merged_data <- bind_rows(existing_data, new_data) |>
    distinct(stationID, datetimeUTC, .keep_all = TRUE)
} else {
  merged_data <- new_data
}

# Write to a temp file then rename, so a failed write can't corrupt the stored data.
tmp_file <- paste0(dataFileLocation, ".tmp")
write.csv(merged_data, tmp_file, row.names = FALSE)
file.rename(tmp_file, dataFileLocation)

print(paste0("Added ", nrow(merged_data) - (if (exists("existing_data")) nrow(existing_data) else 0), " new rows"))
