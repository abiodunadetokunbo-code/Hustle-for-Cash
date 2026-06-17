# Script 05: Clean VIIRS Monthly NTL Panel — Nigeria LGA × Month
# Paper 1a: Nigeria Demonetization / Cash Crunch
#
# Input : data/raw/viirs_monthly_lga_nigeria_2019_2024.csv
#         (GEE/AppEEARS: 774 LGAs × 72 months = 55,728 rows)
# Output: data/outcomes/viirs_ntl_lga_panel.csv   (55,728 rows)
#         data/outcomes/viirs_ntl_state_panel.csv  (2,664 rows)
#
# Shock timeline (naira demonetization):
#   Oct 2022      : CBN announcement
#   Nov-Dec 2022  : transition / early crunch
#   Jan-Feb 2023  : peak cash crunch
#   Mar-Jun 2023  : gradual recovery
#   Jul 2023+     : post-shock

library(readr)
library(dplyr)
library(sf)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"

raw_viirs <- file.path(
  root, "data/raw/viirs_monthly_lga_nigeria_2019_2024.csv"
)
gadm2_shp <- file.path(
  root, "data/raw/shapefiles/gadm_nigeria/gadm41_NGA_2.shp"
)
out_lga   <- file.path(root, "data/outcomes/viirs_ntl_lga_panel.csv")
out_state <- file.path(root, "data/outcomes/viirs_ntl_state_panel.csv")

dir.create(file.path(root, "data/outcomes"), showWarnings = FALSE)

# ── 1. Load raw VIIRS ─────────────────────────────────────────────────────────
cat("Loading raw VIIRS data...\n")
v_raw <- read_csv(raw_viirs, show_col_types = FALSE)
cat("Rows:", nrow(v_raw), "| Cols:", ncol(v_raw), "\n")
cat("Columns:", paste(names(v_raw), collapse = ", "), "\n\n")

# ── 2. Basic clean ────────────────────────────────────────────────────────────
v <- v_raw |>
  select(shapeName, shapeID, date, year, month, ntl_mean = mean) |>
  mutate(
    date     = as.Date(paste0(date, "-01")),
    year     = as.integer(year),
    month    = as.integer(month),
    ntl_mean = as.numeric(ntl_mean)
  ) |>
  arrange(shapeID, year, month)

cat("After basic clean:\n")
cat("  Rows:", nrow(v), "\n")
cat(
  "  NTL range: [", min(v$ntl_mean, na.rm = TRUE), ",",
  max(v$ntl_mean, na.rm = TRUE), "]\n"
)
cat("  NA ntl_mean:", sum(is.na(v$ntl_mean)), "\n\n")

# ── 3. State crosswalk via spatial join ───────────────────────────────────────
# Name-based matching fails for same-name LGAs in different states.
# Strategy: centroid of each geoBoundaries LGA → point-in-polygon on GADM ADM1.
cat("Building LGA → state crosswalk via spatial join...\n")

gb_shp <- file.path(
  root,
  "data/raw/shapefiles/geoboundaries_nga/geoBoundaries-NGA-ADM2.shp"
)
gadm1_shp <- sub("gadm41_NGA_2", "gadm41_NGA_1", gadm2_shp)

gb_lgas <- st_read(gb_shp, quiet = TRUE) |>
  select(shapeName, shapeID)

gadm1 <- st_read(gadm1_shp, quiet = TRUE) |>
  select(state = NAME_1) |>
  st_transform(st_crs(gb_lgas))

gb_centroids <- gb_lgas |>
  st_centroid(of_largest_polygon = TRUE)

crosswalk_sf <- st_join(gb_centroids, gadm1, join = st_within, left = TRUE)

crosswalk <- crosswalk_sf |>
  st_drop_geometry() |>
  select(shapeName, shapeID, state) |>
  distinct(shapeID, .keep_all = TRUE)

matched   <- sum(!is.na(crosswalk$state))
unmatched <- crosswalk |> filter(is.na(state)) |> pull(shapeName)
cat("LGAs matched to state:", matched, "/", nrow(crosswalk), "\n")

if (length(unmatched) > 0) {
  cat(
    "Unmatched (", length(unmatched), "):",
    paste(head(unmatched, 20), collapse = ", "), "\n"
  )
  cat("Applying nearest-feature fallback...\n")
  unmatched_sf <- gb_centroids |> filter(shapeName %in% unmatched)
  nearest_idx  <- st_nearest_feature(unmatched_sf, gadm1)
  fallback <- unmatched_sf |>
    st_drop_geometry() |>
    mutate(state = gadm1$state[nearest_idx]) |>
    select(shapeName, shapeID, state)
  crosswalk <- crosswalk |>
    filter(!is.na(state)) |>
    bind_rows(fallback)
  cat("After fallback:", nrow(crosswalk), "/", nrow(gb_lgas), "\n")
}
cat("\n")

# ── 4. Merge state onto panel ─────────────────────────────────────────────────
state_key <- crosswalk |>
  select(shapeID, state) |>
  distinct(shapeID, .keep_all = TRUE)

v <- v |> left_join(state_key, by = "shapeID")

# ── 5. Log-transform and shock period flags ───────────────────────────────────
v <- v |>
  mutate(
    ln_ntl = log1p(ntl_mean),

    shock_period = case_when(
      year <  2022                     ~ "pre_shock",
      year == 2022 & month <= 9        ~ "pre_shock",
      year == 2022 & month == 10       ~ "announcement",
      year == 2022 & month %in% 11:12  ~ "transition",
      year == 2023 & month %in%  1:2   ~ "peak_crunch",
      year == 2023 & month %in%  3:6   ~ "early_recovery",
      year == 2023 & month >= 7        ~ "post_shock",
      year == 2024                     ~ "post_shock",
      TRUE                             ~ NA_character_
    ),

    shock_window = (year == 2022 & month >= 10) |
      (year == 2023 & month <= 6),

    t_rel = (year - 2023) * 12 + (month - 2)
  )

# ── 6. Save LGA-level panel ───────────────────────────────────────────────────
write_csv(v, out_lga)
cat("Saved LGA panel:", out_lga, "\n")
cat("Rows:", nrow(v), "(expected 55,728)\n\n")

# ── 7. State-month panel ──────────────────────────────────────────────────────
state_panel <- v |>
  filter(!is.na(state)) |>
  group_by(state, year, month, date, shock_period, shock_window, t_rel) |>
  summarise(
    ntl_mean_lgas = mean(ntl_mean, na.rm = TRUE),
    ntl_sum_lgas  = sum(ntl_mean, na.rm = TRUE),
    ln_ntl_mean   = mean(ln_ntl, na.rm = TRUE),
    n_lgas        = n(),
    .groups       = "drop"
  ) |>
  arrange(state, year, month)

write_csv(state_panel, out_state)
cat("Saved state panel:", out_state, "\n")
cat(
  "Rows:", nrow(state_panel),
  "(expected ~37 states x 72 months = 2,664)\n\n"
)

# ── 8. Sanity checks ──────────────────────────────────────────────────────────
cat("=== National mean NTL by shock period ===\n")
v |>
  group_by(shock_period) |>
  summarise(
    mean_ntl = mean(ntl_mean, na.rm = TRUE),
    mean_ln  = mean(ln_ntl, na.rm = TRUE),
    n_obs    = n()
  ) |>
  arrange(factor(shock_period, levels = c(
    "pre_shock", "announcement", "transition",
    "peak_crunch", "early_recovery", "post_shock"
  ))) |>
  print()

cat("\n=== Year-over-year: Jan-Feb 2023 vs Jan-Feb 2022 ===\n")
yoy <- v |>
  filter(year %in% 2022:2023, month %in% 1:2) |>
  group_by(year) |>
  summarise(mean_ln = mean(ln_ntl, na.rm = TRUE), .groups = "drop")
print(yoy)
if (nrow(yoy) == 2) {
  drop_yoy <- yoy$mean_ln[yoy$year == 2022] - yoy$mean_ln[yoy$year == 2023]
  cat(sprintf(
    "YoY drop Jan-Feb: %.4f log pts (2022=%.4f, 2023=%.4f)\n",
    drop_yoy,
    yoy$mean_ln[yoy$year == 2022],
    yoy$mean_ln[yoy$year == 2023]
  ))
}

cat("\nScript 05 complete.\n")
