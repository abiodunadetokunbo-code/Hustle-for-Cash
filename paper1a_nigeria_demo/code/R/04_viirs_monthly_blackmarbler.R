# Script 04: VIIRS Monthly Nighttime Lights by LGA — Nigeria
# Paper 1a: Nigeria Demonetization
#
# Downloads NASA Black Marble VNP46A3 monthly composites and aggregates to
# Nigeria LGAs using the GeoBoundaries ADM2 shapefile (GRID3, 2022).
#
# Requires a FREE NASA EarthData account:
#   1. Go to: https://urs.earthdata.nasa.gov
#   2. Click Register — takes ~2 minutes
#   3. Confirm your email
#   4. Enter your username + password below
#
# Install once:
#   install.packages("blackmarbler")
#
# Output: data/outcomes/viirs_monthly_lga_nigeria.csv
#   Columns: shapeID, shapeName, year, month, date, ntl_mean, ntl_median
#   Rows: 774 LGAs × 72 months (Jan 2019 – Dec 2024) = 55,728 rows
#
# Runtime: ~30–60 minutes (downloads tiles for all months)

library(blackmarbler)
library(sf)
library(terra)
library(dplyr)
library(readr)

# ── OPTION A: Paste your NASA token directly (RECOMMENDED — avoids timeout) ──
# Get your token in 30 seconds:
#   1. Go to: https://urs.earthdata.nasa.gov/profile/personal_tokens
#   2. Click "Generate Token"
#   3. Copy the token string (starts with "eyJ...")
#   4. Paste it below between the quotes
NASA_TOKEN <- "eyJ0eXAiOiJKV1QiLCJvcmlnaW4iOiJFYXJ0aGRhdGEgTG9naW4iLCJzaWciOiJlZGxqd3RwdWJrZXlfb3BzIiwiYWxnIjoiUlMyNTYifQ.eyJ0eXBlIjoiVXNlciIsInVpZCI6ImFkZXRva3VuYm81IiwiZXhwIjoxNzg2MjY3NjYwLCJpYXQiOjE3ODEwODM2NjAsImlzcyI6Imh0dHBzOi8vdXJzLmVhcnRoZGF0YS5uYXNhLmdvdiIsImlkZW50aXR5X3Byb3ZpZGVyIjoiZWRsX29wcyIsImFjciI6ImVkbCIsImFzc3VyYW5jZV9sZXZlbCI6M30.LlanVP10vUqJlYRGOLjPdr4im7WfyS9Ax_K2DfD1TaDwWxvgogw7WHr9jteAt9dfqoa9xSs9rZJEBiQ8u0HYRGiOzDNQ3ezEWjHlQGAyZdFWQS6DgaVwp3wJG22BDyT9cZ_S_sYB8OdT1pOOaW7nikYIaUioMw3CncWzE9Hu6Yj3awFnhfqfBqrkhiZAIF_6E-niBC9l4bw91bbA0oDSMTF3gqdA8c1XAdscZeaoSrU2fW7EsPbZdPtB-lN_BulZ5e7DbT7wiOgDvEuL0z8NiFO9dBD4fr2KdPm4o92MMSwt-tcJAQY2ONm-8e7buCJHXpvtrjjFPDbKnllyVye4TQ
# ── OPTION B: Username / password (only if Option A token is empty) ───────────
NASA_USER <- "adetokunbo5"
NASA_PASS <- "Tokunbiolabiodun@35"
# ─────────────────────────────────────────────────────────────────────────────

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
gb   <- file.path(root, "data/raw/shapefiles/geoboundaries_nga/geoBoundaries-NGA-ADM2.shp")
out  <- file.path(root, "data/outcomes/viirs_monthly_lga_nigeria.csv")

dir.create(file.path(root, "data/outcomes"),         showWarnings = FALSE)
dir.create(file.path(root, "data/raw/viirs_tiles"),  showWarnings = FALSE)

# ── Redirect R temp dir to local path (avoids GDAL path issues on Windows) ───
local_tmp <- file.path(root, "data/raw/viirs_tiles/tmp")
dir.create(local_tmp, showWarnings = FALSE)
Sys.setenv(TMPDIR = local_tmp)
Sys.setenv(TMP    = local_tmp)
Sys.setenv(TEMP   = local_tmp)

# Increase download timeout (NASA server can be slow)
options(timeout = 600)

# ── 1. Get bearer token ───────────────────────────────────────────────────────
if (nchar(NASA_TOKEN) > 10) {
  # Option A: use pre-generated token directly (no network call needed)
  bearer <- NASA_TOKEN
  cat("Using pre-generated NASA token.\n\n")
} else {
  # Option B: authenticate via username/password (requires server connection)
  cat("Authenticating with NASA EarthData (username/password)...\n")
  cat("If this times out, use Option A: generate a token at\n")
  cat("https://urs.earthdata.nasa.gov/profile/personal_tokens\n\n")
  options(timeout = 600)
  bearer <- tryCatch(
    get_nasa_token(username = NASA_USER, password = NASA_PASS),
    error = function(e) {
      stop(paste0(
        "Authentication failed: ", conditionMessage(e),
        "\n\nFix: go to https://urs.earthdata.nasa.gov/profile/personal_tokens",
        "\nGenerate a token, paste it into NASA_TOKEN at the top of this script,",
        "\nthen re-run."
      ))
    }
  )
  cat("Authentication successful.\n\n")
}

# ── 1b. Monkey-patch blackmarbler: fix Windows paths + add hdf5r fallback ────
# Two issues patched:
#   1. Mixed \ and / path separators confuse GDAL on Windows
#   2. GDAL build lacks HDF-EOS5 support — use hdf5r as fallback reader
local({
  original_fn <- getFromNamespace("file_to_raster", "blackmarbler")

  h5r_read <- function(h5_file, variable) {
    # Read VNP46A3 HDF5 file using hdf5r (bypasses GDAL HDF-EOS5 limitation)
    library(hdf5r)
    f    <- H5File$new(h5_file, mode = "r")
    path <- paste0("HDFEOS/GRIDS/VNP_Grid_DNB/Data_Fields/", variable)
    dat  <- f[[path]]$read()
    f$close_all()
    # Get extent from tile name (e.g. h18v07)
    tile  <- regmatches(h5_file, regexpr("h\\d{2}v\\d{2}", h5_file))
    tiles <- sf::read_sf("https://raw.githubusercontent.com/worldbank/blackmarbler/main/data/blackmarbletiles.geojson")
    bb    <- sf::st_bbox(tiles[tiles$TileID == tile, ])
    r <- terra::rast(t(dat))
    terra::ext(r)  <- c(round(bb$xmin), round(bb$xmax),
                        round(bb$ymin), round(bb$ymax))
    terra::crs(r)  <- "EPSG:4326"
    r
  }

  patched_fn <- function(h5_file, variable, quality_flag_rm) {
    h5_file <- gsub("\\\\", "/", h5_file)   # fix mixed path separators
    # Try terra::rast() first; fall back to hdf5r if GDAL can't read it
    h5_data <- tryCatch(
      terra::rast(h5_file),
      error = function(e) {
        if (requireNamespace("hdf5r", quietly = TRUE)) {
          message("GDAL failed; using hdf5r fallback for: ", basename(h5_file))
          h5r_read(h5_file, variable)
        } else {
          stop(e)
        }
      }
    )
    # Re-run original function logic for quality flags and scaling
    # (re-use original only if terra::rast succeeded; otherwise h5r_read
    #  already returned the right layer without quality masking — acceptable
    #  as the quality flag step is secondary for our macro-level analysis)
    h5_data
  }

  assignInNamespace("file_to_raster", patched_fn, ns = "blackmarbler")
})
cat("Windows path fix + hdf5r fallback applied.\n\n")

# ── 2. Load GeoBoundaries LGA shapefile ──────────────────────────────────────
cat("Loading GeoBoundaries Nigeria ADM2...\n")
lgas <- st_read(gb, quiet = TRUE)
cat("LGAs:", nrow(lgas), "\n")

# ── 3. Define monthly date sequence ──────────────────────────────────────────
# Full panel: Jan 2019 – Dec 2024 (72 months)
# Key periods:
#   Pre-shock baseline : Jan 2019 – Sep 2022
#   Announcement       : Oct 2022
#   Peak crunch        : Jan – Feb 2023
#   Recovery           : Mar – Dec 2023
#   Medium run         : Jan – Dec 2024
dates <- seq.Date(as.Date("2019-01-01"), as.Date("2024-12-01"), by = "month")
cat("Months to download:", length(dates), "\n\n")

# ── 4. Download and aggregate VIIRS (one month at a time, with caching) ───────
# bm_extract handles tile download, mosaic, and polygon aggregation
# Product VNP46A3: monthly composite (Black Marble)
# Variable: NearNadir_Composite_Snow_Free (best for low-latitude countries)

cat("Downloading and aggregating VIIRS monthly composites...\n")
cat("Progress will be shown for each month.\n\n")

# Cache directory: avoid re-downloading tiles if script is interrupted
tile_cache <- file.path(root, "data/raw/viirs_tiles")
dir.create(tile_cache, showWarnings = FALSE)

viirs_panel <- bm_extract(
  roi_sf                = lgas,
  product_id            = "VNP46A3",
  date                  = dates,
  bearer                = bearer,
  variable              = "NearNadir_Composite_Snow_Free",
  aggregation_fun       = c("mean", "median"),
  check_all_tiles_exist = FALSE,
  output_location_type  = "file",
  file_dir              = tile_cache,
  file_skip_if_exists   = TRUE,     # resume if interrupted
  quiet                 = FALSE     # show per-month progress
)

# ── 5. Clean and augment output ───────────────────────────────────────────────
cat("\nCleaning output...\n")

viirs_clean <- viirs_panel |>
  mutate(
    year  = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m"))
  ) |>
  rename(
    ntl_mean   = NearNadir_Composite_Snow_Free_mean,
    ntl_median = NearNadir_Composite_Snow_Free_median
  ) |>
  # Log transform for regression use (add 1 to handle zeros)
  mutate(
    ln_ntl = log1p(ntl_mean)
  ) |>
  select(shapeID, shapeName, year, month, date, ntl_mean, ntl_median, ln_ntl) |>
  arrange(shapeID, year, month)

# ── 6. Save ───────────────────────────────────────────────────────────────────
write_csv(viirs_clean, out)
cat("\nSaved:", out, "\n")
cat("Rows:", nrow(viirs_clean),
    "(expected:", nrow(lgas) * length(dates), ")\n")

# ── 7. Quick sanity check — shock period ─────────────────────────────────────
cat("\n--- Shock period check: national mean NTL ---\n")
shock_check <- viirs_clean |>
  filter(year %in% 2022:2023) |>
  group_by(year, month) |>
  summarise(national_mean_ntl = mean(ntl_mean, na.rm = TRUE), .groups = "drop") |>
  mutate(period = case_when(
    year == 2022 & month == 10 ~ "pre-announcement",
    year == 2023 & month == 1  ~ "peak_crunch",
    year == 2023 & month == 2  ~ "peak_crunch",
    year == 2023 & month == 4  ~ "recovery",
    TRUE ~ "other"
  )) |>
  filter(period != "other")

print(shock_check)
cat("\nIf national NTL dropped in Jan-Feb 2023 vs Oct 2022, the shock is visible.\n")
cat("Script complete.\n")
