# Script 03: Road Distance to Commercial Hub — Instrument 2
# Paper 1a: Nigeria Demonetization
#
# Computes geodesic distance from each LGA centroid to the nearest of
# five major Nigerian commercial hubs. Used as Instrument 2.
#
# Two methods:
#   Method A: Pure base R (Haversine) — no extra packages, uses LGA centroids
#             derived from LSMS household GPS coordinates.
#   Method B: sf package — uses GADM shapefile centroids (more precise).
#
# Method A runs immediately. Method B requires sf (install if not yet done).
#
# Output: data/instruments/road_dist_hub_lga.csv

library(dplyr)
library(readr)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
out  <- file.path(root, "data/instruments/road_dist_hub_lga.csv")

# Commercial hub coordinates (lon, lat)
HUBS <- list(
  lagos        = c(3.3841,  6.4550),
  abuja        = c(7.4951,  9.0579),
  kano         = c(8.5167, 12.0000),
  portharcourt = c(7.0494,  4.8156),
  onitsha      = c(6.7833,  6.1667)
)

# ── Haversine distance function (base R, no packages needed) ─────────────────
haversine_km <- function(lon1, lat1, lon2, lat2) {
  R <- 6371
  d_lon <- (lon2 - lon1) * pi / 180
  d_lat <- (lat2 - lat1) * pi / 180
  a <- sin(d_lat/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(d_lon/2)^2
  2 * R * asin(pmin(1, sqrt(a)))
}

# ── Method A: Centroids from LSMS/NLPS household GPS ─────────────────────────
method_a <- function() {
  cat("Method A: LGA centroids from NLPS household locations\n")

  nlps <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")

  # Use Round 6 sect_a which has geographic identifiers
  # Note: NLPS uses state×lga codes, not GPS coordinates.
  # We use the LGA name to approximate centroids via lookup table.
  sa_r6 <- read_csv(file.path(nlps, "p2r6_sect_a_2_5_6_8_11b_12.csv"),
                    show_col_types = FALSE) |>
    select(hhid, zone, state, lga, sector)

  # GHSP Wave 5 secta has ea GPS offset coordinates
  ghsp_sa <- read_csv(
    file.path(root,
      "data/raw/lsms_isa/wave5_ghsp/Post Harvest Wave 5/Household/secta_harvestw5.csv"),
    show_col_types = FALSE
  )

  cat("  GHSP secta columns:", paste(names(ghsp_sa)[1:10], collapse=", "), "\n")

  # Check if GPS columns exist
  gps_cols <- names(ghsp_sa)[grepl("gps|lat|lon|coord|GPS", names(ghsp_sa),
                                     ignore.case=TRUE)]
  cat("  GPS-related columns found:", paste(gps_cols, collapse=", "), "\n")

  if (length(gps_cols) == 0) {
    cat("  No GPS columns in GHSP secta — using Method B (sf) instead.\n")
    return(NULL)
  }

  # Aggregate EA GPS to LGA centroids
  centroids <- ghsp_sa |>
    filter(!is.na(get(gps_cols[1])), !is.na(get(gps_cols[2]))) |>
    group_by(lga) |>
    summarise(
      state      = first(state),
      centroid_lon = mean(get(gps_cols[1]), na.rm = TRUE),
      centroid_lat = mean(get(gps_cols[2]), na.rm = TRUE),
      .groups = "drop"
    )

  return(centroids)
}

# ── Method B: Centroids from GADM shapefile (sf package) ─────────────────────
method_b <- function() {
  cat("Method B: LGA centroids from GADM shapefile (requires sf)\n")

  if (!requireNamespace("sf", quietly = TRUE)) {
    cat("  sf not installed. Run: install.packages('sf')\n")
    return(NULL)
  }

  library(sf)
  gadm <- file.path(root, "data/raw/shapefiles/gadm_nigeria/gadm41_NGA_2.shp")
  lgas <- st_read(gadm, quiet = TRUE)

  # Project to UTM32N for accurate centroids
  lgas_utm  <- st_transform(lgas, 32632)
  cents_wgs  <- st_transform(st_centroid(lgas_utm), 4326)

  centroids <- tibble(
    GID_2       = lgas$GID_2,
    lga_name    = lgas$NAME_2,
    state_name  = lgas$NAME_1,
    centroid_lon = st_coordinates(cents_wgs)[, 1],
    centroid_lat = st_coordinates(cents_wgs)[, 2]
  )

  cat("  Loaded", nrow(centroids), "LGA centroids from GADM\n")
  return(centroids)
}

# ── Main: compute distances ───────────────────────────────────────────────────
cat("Computing road distances to commercial hubs...\n")

# Try Method B first (more accurate), fall back to Method A
centroids <- method_b()
if (is.null(centroids)) centroids <- method_a()
if (is.null(centroids)) stop("Both methods failed. Check that sf is installed.")

# Add hub distances
for (hub_name in names(HUBS)) {
  col_name <- paste0("dist_", hub_name, "_km")
  centroids[[col_name]] <- haversine_km(
    centroids$centroid_lon, centroids$centroid_lat,
    HUBS[[hub_name]][1], HUBS[[hub_name]][2]
  )
}

# Minimum distance and nearest hub
dist_cols <- paste0("dist_", names(HUBS), "_km")
centroids <- centroids |>
  mutate(
    min_dist_km  = pmin(!!!syms(dist_cols), na.rm = TRUE),
    nearest_hub  = names(HUBS)[
      apply(select(centroids, all_of(dist_cols)), 1, which.min)
    ]
  )

# Save
write_csv(centroids, out)
cat("\nSaved:", out, "\n")
cat("LGAs:", nrow(centroids), "\n")

cat("\nSummary of distances to nearest hub:\n")
print(summary(centroids$min_dist_km))

cat("\nLGAs by nearest hub:\n")
print(table(centroids$nearest_hub))

cat("\nScript complete.\n")
