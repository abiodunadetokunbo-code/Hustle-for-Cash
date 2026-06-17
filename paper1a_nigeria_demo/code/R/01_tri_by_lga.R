# Script 01: Terrain Ruggedness Index (TRI) by LGA — Nigeria
# Paper 1a: Nigeria Demonetization — Instrument 1
#
# Downloads SRTM elevation tiles automatically (no account, uses Amazon terrain
# tiles via the elevatr package), computes TRI, and aggregates to 774 LGAs.
#
# Install once (run this section once, then comment out):
# install.packages(c("sf","terra","exactextractr","elevatr","dplyr","readr"))
#
# Runtime: ~10 minutes (download + compute)
# Output:  data/instruments/tri_lga_nigeria.csv

library(sf)
library(terra)
library(exactextractr)
library(elevatr)
library(dplyr)
library(readr)

# ── Paths ─────────────────────────────────────────────────────────────────────
root  <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
gadm  <- file.path(root, "data/raw/shapefiles/gadm_nigeria/gadm41_NGA_2.shp")
out   <- file.path(root, "data/instruments/tri_lga_nigeria.csv")

# ── 1. Load LGA shapefile ─────────────────────────────────────────────────────
cat("Loading GADM Nigeria LGA shapefile...\n")
lgas <- st_read(gadm, quiet = TRUE)
cat("LGAs:", nrow(lgas), "\n")

# ── 2. Download SRTM elevation for Nigeria ────────────────────────────────────
# elevatr downloads Mapzen/AWS terrain tiles — no account required
# z = zoom level: 7 = ~1km resolution (fast), 9 = ~250m (better for TRI)
elev_cache <- file.path(root, "data/instruments/nigeria_srtm_z8.tif")

if (file.exists(elev_cache)) {
  cat("Loading cached SRTM raster:", elev_cache, "\n")
  elev <- rast(elev_cache)
} else {
  cat("Downloading SRTM elevation tiles (z=8, ~500m)...\n")
  cat("This takes 3-5 minutes. Will be cached for future runs.\n")
  elev_rast <- get_elev_raster(
    locations = lgas, z = 8, src = "aws", clip = "bbox"
  )
  elev <- rast(elev_rast)
  writeRaster(elev, elev_cache, overwrite = TRUE)
  cat("Cached to:", elev_cache, "\n")
}
cat("Elevation raster downloaded:", nrow(elev), "rows x", ncol(elev), "cols\n")
cat("Elevation range:", round(minmax(elev)[1], 0), "–", round(minmax(elev)[2], 0), "m\n")

# ── 3. Compute Terrain Ruggedness Index ───────────────────────────────────────
# TRI (Riley et al. 1999) = focal standard deviation in a 3×3 neighbourhood
# This is the standard implementation used in African development economics
# (Nunn & Puga 2012, Michalopoulos & Papaioannou, etc.)
cat("Computing TRI (3x3 focal standard deviation)...\n")

tri <- focal(
  elev,
  w   = matrix(1, 3, 3),  # 3x3 window
  fun = sd,                # standard deviation = TRI approximation
  na.rm = TRUE
)
names(tri) <- "tri"

cat("TRI range:", round(minmax(tri)[1], 2), "–", round(minmax(tri)[2], 2), "\n")

# ── 4. Zonal statistics: mean TRI per LGA ─────────────────────────────────────
cat("Aggregating TRI to", nrow(lgas), "LGAs...\n")

# Reproject LGAs to match raster CRS if needed
lgas_proj <- st_transform(lgas, crs(tri))

# exactextractr is fast and handles partial pixel coverage correctly
tri_stats <- exact_extract(
  tri,
  lgas_proj,
  fun = c("mean", "stdev", "count")
)

# ── 5. Assemble output dataframe ──────────────────────────────────────────────
cat("Assembling results...\n")

# Compute LGA centroids (project to UTM first for accuracy, then back to WGS84)
lgas_utm  <- st_transform(lgas, 32632)
centroids <- st_transform(st_centroid(lgas_utm), 4326)

result <- lgas |>
  st_drop_geometry() |>
  select(GID_2, NAME_2, NAME_1) |>
  rename(lga_name = NAME_2, state_name = NAME_1) |>
  mutate(
    centroid_lon = st_coordinates(centroids)[, 1],
    centroid_lat = st_coordinates(centroids)[, 2],
    mean_tri     = tri_stats$mean,
    sd_tri       = tri_stats$stdev,
    pixel_count  = tri_stats$count
  ) |>
  mutate(
    # Standardise for use as instrument
    tri_std = (mean_tri - mean(mean_tri, na.rm = TRUE)) /
               sd(mean_tri, na.rm = TRUE)
  )

# ── 6. Save ───────────────────────────────────────────────────────────────────
write_csv(result, out)
cat("\nSaved:", out, "\n")
cat("Rows:", nrow(result), "\n")

# ── 7. Diagnostics ────────────────────────────────────────────────────────────
cat("\n--- TRI Summary ---\n")
print(summary(result$mean_tri))

cat("\nTop 10 most rugged LGAs (low agent density expected):\n")
result |>
  arrange(desc(mean_tri)) |>
  slice_head(n = 10) |>
  select(lga_name, state_name, mean_tri) |>
  print()

cat("\nTop 10 flattest LGAs (high agent density expected):\n")
result |>
  arrange(mean_tri) |>
  slice_head(n = 10) |>
  select(lga_name, state_name, mean_tri) |>
  print()

cat("\nScript complete.\n")
