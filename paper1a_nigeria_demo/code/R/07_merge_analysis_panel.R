# Script 07: Build Main Analysis Panel
# Paper 1a: Nigeria Demonetization / Cash Crunch
#
# Merges:
#   VIIRS NTL outcome    → data/outcomes/viirs_ntl_lga_panel.csv
#   Road distance IV     → data/instruments/road_dist_hub_lga.csv
#   TRI (terrain)        → data/instruments/tri_lga_nigeria.csv
#   Pre-shock fintech    → data/instruments/preschock_fintech_state.csv
#   WBES state controls  → data/processed/wbes_nga2014_state.csv  (optional)
#
# Output: LGA-month panel (55,728 rows) and state-month panel (2,664 rows)
#         written to data/processed/

library(readr)
library(dplyr)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"

viirs_lga   <- file.path(root, "data/outcomes/viirs_ntl_lga_panel.csv")
viirs_state <- file.path(root, "data/outcomes/viirs_ntl_state_panel.csv")
road_dist   <- file.path(root, "data/instruments/road_dist_hub_lga.csv")
tri_lga     <- file.path(root, "data/instruments/tri_lga_nigeria.csv")
fintech_st  <- file.path(root, "data/instruments/preschock_fintech_state.csv")
wbes_st     <- file.path(root, "data/processed/wbes2014_state.csv")

out_lga     <- file.path(root, "data/processed/analysis_panel_lga.csv")
out_state   <- file.path(root, "data/processed/analysis_panel_state.csv")

dir.create(file.path(root, "data/processed"), showWarnings = FALSE)

# ── NBS state code → GADM state name crosswalk (alphabetical, 1-37) ──────────
nbs_state_names <- c(
  "Abia", "Adamawa", "Akwa Ibom", "Anambra", "Bauchi",
  "Bayelsa", "Benue", "Borno", "Cross River", "Delta",
  "Ebonyi", "Edo", "Ekiti", "Enugu", "Gombe",
  "Imo", "Jigawa", "Kaduna", "Kano", "Katsina",
  "Kebbi", "Kogi", "Kwara", "Lagos", "Nasarawa",
  "Niger", "Ogun", "Ondo", "Osun", "Oyo",
  "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe",
  "Zamfara", "Federal Capital Territory"
)
nbs_lookup <- tibble(
  state_code = 1:37,
  state      = nbs_state_names
)

# ── 1. Load VIIRS panels ──────────────────────────────────────────────────────
cat("Loading VIIRS panels...\n")
v_lga   <- read_csv(viirs_lga,   show_col_types = FALSE)
v_state <- read_csv(viirs_state, show_col_types = FALSE)
cat("LGA panel:", nrow(v_lga), "rows\n")
cat("State panel:", nrow(v_state), "rows\n\n")

# ── 2. Load and join road distance (on lga_name + state_name) ────────────────
cat("Loading road distance instrument...\n")
rd <- read_csv(road_dist, show_col_types = FALSE) |>
  select(
    lga_name, state_name,
    dist_lagos_km, dist_abuja_km, dist_kano_km,
    dist_portharcourt_km, min_dist_km, nearest_hub
  ) |>
  distinct(lga_name, state_name, .keep_all = TRUE)

cat("Road dist rows (deduped):", nrow(rd), "\n")

# ── 3. Load and join TRI (on lga_name + state_name) ──────────────────────────
cat("Loading terrain ruggedness index...\n")
tri <- read_csv(tri_lga, show_col_types = FALSE) |>
  select(lga_name, state_name, mean_tri, sd_tri, tri_std) |>
  distinct(lga_name, state_name, .keep_all = TRUE)

cat("TRI rows (deduped):", nrow(tri), "\n\n")

# ── 4. Load fintech proxy (decode state_code → state name) ───────────────────
cat("Loading pre-shock fintech proxy...\n")
ft <- read_csv(fintech_st, show_col_types = FALSE) |>
  left_join(nbs_lookup, by = "state_code") |>
  select(state, pct_bank_account, pct_mobile_money,
         pct_mobile_phone_ussd, pct_mobile_banking_app,
         pct_fintech_index, pct_any_digital_pay)

cat("Fintech states:", nrow(ft), "\n")
cat("Unmatched state codes:", sum(is.na(ft$state)), "\n\n")

# ── 5. Create compound LGA join key for road_dist and TRI ────────────────────
# VIIRS state names from GADM, instruments from GADM → same NAME_1 values
v_lga <- v_lga |>
  mutate(join_key = paste0(shapeName, "||", state))

rd <- rd |>
  mutate(join_key = paste0(lga_name, "||", state_name))

tri <- tri |>
  mutate(join_key = paste0(lga_name, "||", state_name))

# ── 6. Build LGA analysis panel ───────────────────────────────────────────────
cat("Building LGA analysis panel...\n")

rd_slim  <- rd  |> select(-lga_name, -state_name)
tri_slim <- tri |> select(-lga_name, -state_name)

panel_lga <- v_lga |>
  left_join(rd_slim,  by = "join_key") |>
  left_join(tri_slim, by = "join_key") |>
  left_join(ft,       by = "state") |>
  select(-join_key)

cat("LGA panel rows:", nrow(panel_lga), "(expected 55,728)\n")
cat(
  "Road dist matched: ",
  round(mean(!is.na(panel_lga$min_dist_km)) * 100, 1), "%\n"
)
cat(
  "Fintech matched:   ",
  round(mean(!is.na(panel_lga$pct_fintech_index)) * 100, 1), "%\n\n"
)

# ── 7. Build state analysis panel ────────────────────────────────────────────
cat("Building state analysis panel...\n")
panel_state <- v_state |>
  left_join(ft, by = "state")

# Merge WBES state controls if available
if (file.exists(wbes_st)) {
  wbes <- read_csv(wbes_st, show_col_types = FALSE)
  cat("Merging WBES state controls (", nrow(wbes), "states)...\n")
  panel_lga   <- panel_lga   |> left_join(wbes, by = "state")
  panel_state <- panel_state |> left_join(wbes, by = "state")
} else {
  cat("WBES controls not yet available (run 06_wbes_clean.R after download).\n")
}

# ── 8. DiD treatment variable ─────────────────────────────────────────────────
# High-fintech states = more insulated from the cash crunch
# Treatment = above-median pre-shock fintech index (time-invariant)
med_ft <- median(panel_lga$pct_fintech_index, na.rm = TRUE)
panel_lga <- panel_lga |>
  mutate(
    treat = as.integer(pct_fintech_index >= med_ft),
    post  = as.integer(year == 2023 & month %in% 1:2),
    did   = treat * post
  )

med_ft_s <- median(panel_state$pct_fintech_index, na.rm = TRUE)
panel_state <- panel_state |>
  mutate(
    treat = as.integer(pct_fintech_index >= med_ft_s),
    post  = as.integer(year == 2023 & month %in% 1:2),
    did   = treat * post
  )

# ── 9. Save ───────────────────────────────────────────────────────────────────
write_csv(panel_lga,   out_lga)
write_csv(panel_state, out_state)

cat("Saved LGA panel:   ", out_lga,   "\n")
cat("Saved state panel: ", out_state, "\n\n")

# ── 10. Naive 2x2 DiD preview ─────────────────────────────────────────────────
cat("=== Naive 2x2 DiD: Jan-Feb 2023 vs Jan-Feb 2022 ===\n")
cat("(+treat = high fintech = less cash-dependent = smaller NTL drop)\n\n")

panel_state |>
  filter(year %in% c(2022, 2023), month %in% 1:2, !is.na(treat)) |>
  group_by(treat, year) |>
  summarise(mean_ln_ntl = mean(ln_ntl_mean, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = mean_ln_ntl,
                     names_prefix = "y") |>
  mutate(
    delta      = y2023 - y2022,
    treat_lbl  = ifelse(
      treat == 1, "High-fintech (treat)", "Low-fintech (ctrl)"
    )
  ) |>
  select(treat_lbl, y2022, y2023, delta) |>
  print()

did_est <- panel_state |>
  filter(year %in% c(2022, 2023), month %in% 1:2, !is.na(treat)) |>
  group_by(treat, year) |>
  summarise(m = mean(ln_ntl_mean, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = c(treat, year), values_from = m)

if (ncol(did_est) == 4) {
  did_val <- (did_est[[4]] - did_est[[3]]) - (did_est[[2]] - did_est[[1]])
  cat(sprintf("\nDiD estimate: %.4f log points\n", did_val))
  cat("(positive = high-fintech states lost less NTL during peak crunch)\n")
}

cat("\nScript 07 complete.\n")
