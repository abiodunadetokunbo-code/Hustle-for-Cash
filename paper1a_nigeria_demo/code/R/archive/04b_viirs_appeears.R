# Script 04b: VIIRS Monthly Nighttime Lights via NASA AppEEARS API
# Paper 1a: Nigeria Demonetization
#
# AppEEARS (Application for Extracting and Exploring Analysis Ready Samples)
# processes NASA data SERVER-SIDE and delivers a CSV — no local HDF5 reading,
# no LAADS DAAC authorization needed. Uses the same NASA EarthData credentials.
#
# Workflow:
#   Step 1 (this script): Submit the request → get a task_id
#   Step 2 (run later):   Poll until done → download CSV
#
# Output: data/outcomes/viirs_monthly_lga_nigeria.csv
# Runtime: Submit takes ~1 minute. Processing takes 2-24 hours on NASA's servers.

library(httr2)
library(sf)
library(dplyr)
library(readr)
library(jsonlite)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
gb   <- file.path(root, "data/raw/shapefiles/geoboundaries_nga/geoBoundaries-NGA-ADM2.shp")
out  <- file.path(root, "data/outcomes/viirs_monthly_lga_nigeria.csv")
task_file <- file.path(root, "docs/appeears_task_id.txt")

dir.create(file.path(root, "data/outcomes"), showWarnings = FALSE)

# ── Credentials ───────────────────────────────────────────────────────────────
NASA_USER <- "adetokunbo5"
NASA_PASS <- "Tokunbiolabiodun@35"

# ── AppEEARS API base ─────────────────────────────────────────────────────────
BASE <- "https://appeears.earthdatacloud.nasa.gov/api"

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 1: Authenticate and submit the extraction task
# ═══════════════════════════════════════════════════════════════════════════════
submit_task <- function() {
  cat("Step 1: Authenticating with AppEEARS...\n")
  login <- request(paste0(BASE, "/login")) |>
    req_auth_basic(NASA_USER, NASA_PASS) |>
    req_timeout(60) |>
    req_perform()

  token <- resp_body_json(login)$token
  cat("AppEEARS authentication successful.\n\n")

  # Load LGA boundaries as GeoJSON
  cat("Loading GeoBoundaries...\n")
  lgas <- st_read(gb, quiet = TRUE)

  # AppEEARS expects a GeoJSON FeatureCollection
  geojson_tmp <- tempfile(fileext = ".geojson")
  st_write(lgas, geojson_tmp, driver = "GeoJSON", quiet = TRUE)
  geojson_str <- readLines(geojson_tmp, warn = FALSE) |> paste(collapse = "")
  unlink(geojson_tmp)

  # Build task specification
  # VNP46A3: VIIRS/NPP Monthly Lunar BRDF-Adjusted NTL, Version 001
  # Layer: NearNadir_Composite_Snow_Free (best for tropical/equatorial regions)
  task <- list(
    task_type    = "area",
    task_name    = "VIIRS_NTL_Nigeria_LGA_Monthly_2019_2024",
    params = list(
      dates = list(
        list(startDate = "01-01-2019", endDate = "12-01-2024")
      ),
      layers = list(
        list(
          product  = "VNP46A3.001",
          layer    = "NearNadir_Composite_Snow_Free"
        )
      ),
      output = list(format = list(type = "geotiff"), projection = "geographic"),
      geo = list(
        type     = "FeatureCollection",
        features = fromJSON(geojson_str)$features
      )
    )
  )

  cat("Submitting extraction task to AppEEARS...\n")
  cat("Task: VNP46A3 Monthly NTL for 774 Nigeria LGAs, Jan 2019 – Dec 2024\n\n")

  resp <- request(paste0(BASE, "/task")) |>
    req_auth_bearer_token(token) |>
    req_headers("Content-Type" = "application/json") |>
    req_body_raw(toJSON(task, auto_unbox = TRUE)) |>
    req_timeout(120) |>
    req_perform()

  result <- resp_body_json(resp)
  task_id <- result$task_id
  cat("Task submitted successfully!\n")
  cat("Task ID:", task_id, "\n\n")

  # Save task_id for later retrieval
  writeLines(c(task_id, token), task_file)
  cat("Task ID saved to:", task_file, "\n")
  cat("Token saved (needed for download step).\n\n")

  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
  cat("NEXT STEP: Check task status and download results.\n")
  cat("Processing takes 2–24 hours on NASA's servers.\n")
  cat("Run this script again with --download flag when ready:\n")
  cat("  Rscript code/R/04b_viirs_appeears.R --download\n")
  cat("Or check status at: https://appeears.earthdatacloud.nasa.gov/task/", task_id, "\n")
  cat("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")

  return(task_id)
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 2: Poll status and download when complete
# ═══════════════════════════════════════════════════════════════════════════════
download_results <- function() {
  if (!file.exists(task_file)) {
    stop("No task ID found. Run Step 1 first (without --download flag).")
  }

  lines   <- readLines(task_file)
  task_id <- lines[1]
  token   <- lines[2]

  cat("Checking AppEEARS task status:", task_id, "\n")

  # Check status
  status_resp <- request(paste0(BASE, "/task/", task_id)) |>
    req_auth_bearer_token(token) |>
    req_timeout(30) |>
    req_perform()

  status <- resp_body_json(status_resp)
  cat("Status:", status$status, "\n")
  cat("Progress:", status$progress, "%\n\n")

  if (status$status != "done") {
    cat("Task not complete yet. Check back later.\n")
    cat("Status URL: https://appeears.earthdatacloud.nasa.gov/task/", task_id, "\n")
    return(invisible(NULL))
  }

  # List output files
  files_resp <- request(paste0(BASE, "/bundle/", task_id)) |>
    req_auth_bearer_token(token) |>
    req_timeout(30) |>
    req_perform()

  files <- resp_body_json(files_resp)$files
  cat("Files available:", length(files), "\n")

  # Find the statistics CSV (the main output we want)
  csv_files <- Filter(function(f) grepl("\\.csv$", f$file_name), files)
  cat("CSV files found:", length(csv_files), "\n")

  if (length(csv_files) == 0) {
    cat("No CSV files yet. Try again in a few minutes.\n")
    return(invisible(NULL))
  }

  # Download the statistics CSV
  dl_dir <- file.path(root, "data/raw/appeears_output")
  dir.create(dl_dir, showWarnings = FALSE)

  for (f in csv_files) {
    dest <- file.path(dl_dir, basename(f$file_name))
    cat("Downloading:", f$file_name, "→", dest, "\n")
    request(paste0(BASE, "/bundle/", task_id, "/", f$file_id)) |>
      req_auth_bearer_token(token) |>
      req_timeout(300) |>
      req_perform(path = dest)
  }

  cat("\nDownload complete. Files in:", dl_dir, "\n")
  cat("Now run the aggregation step...\n")
  aggregate_csv(dl_dir)
}

# ═══════════════════════════════════════════════════════════════════════════════
# STEP 3: Aggregate AppEEARS output to LGA × month panel
# ═══════════════════════════════════════════════════════════════════════════════
aggregate_csv <- function(dl_dir) {
  # AppEEARS delivers one CSV per date or one combined CSV
  # Read, aggregate by LGA + month, save final panel
  csvs <- list.files(dl_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csvs) == 0) {
    cat("No CSV files to aggregate yet.\n")
    return(invisible(NULL))
  }

  df <- lapply(csvs, read_csv, show_col_types = FALSE) |> bind_rows()
  cat("Rows loaded:", nrow(df), "\n")
  cat("Columns:", paste(names(df), collapse = ", "), "\n")

  # Save raw output (column names vary — inspect before further processing)
  write_csv(df, out)
  cat("Saved combined output to:", out, "\n")
}

# ── Main ──────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)

if ("--download" %in% args) {
  download_results()
} else {
  submit_task()
}
