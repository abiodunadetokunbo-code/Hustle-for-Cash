# Script 08: Build 3-Round Enterprise Panel (R5 / R7 / R11)
# Merges state-level fintech treatment; saves analysis-ready firm panel.

library(readr); library(dplyr); library(tidyr)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")

# ── 1. Load fintech treatment (state_code → fintech_std) ─────────────────────
ft_raw <- read_csv(
  file.path(root, "data/instruments/preschock_fintech_state.csv"),
  show_col_types = FALSE
)

ft <- ft_raw |>
  mutate(
    fintech_std = as.numeric(scale(pct_fintech_index)),
    treat_hi    = as.integer(fintech_std >= 0)
  ) |>
  select(state_code, fintech_std, treat_hi, pct_fintech_index,
         pct_bank_account, pct_mobile_money)

# ── 2. Load each round ────────────────────────────────────────────────────────
read_geo <- function(f) {
  read_csv(f, show_col_types = FALSE) |>
    select(hhid, zone, state, lga, sector) |>
    distinct(hhid, .keep_all = TRUE)
}

# Round 5 — Aug 2022 (pre-shock baseline)
r5_raw <- read_csv(
  file.path(nlps, "p2r5_sect_a_2_5_6_9a_11b_13_12.csv"),
  show_col_types = FALSE
)
r5 <- r5_raw |>
  select(hhid, zone, state, lga, sector,
         active    = s13q1,          # 1=operating, 2=not
         sales_cat = s13q3,          # 1-5 ordinal bracket
         workers   = s13q4) |>       # 1-5 ordinal bracket
  mutate(
    round    = "R5",
    round_n  = 5L,
    active   = as.integer(active == 1),
    # Map ordinal sales bracket to midpoint (₦k/week, rough)
    # 1:<5k, 2:5-20k, 3:20-100k, 4:100-500k, 5:>500k
    ln_sales = log(case_when(
      sales_cat == 1 ~ 2500,
      sales_cat == 2 ~ 12500,
      sales_cat == 3 ~ 60000,
      sales_cat == 4 ~ 300000,
      sales_cat == 5 ~ 750000,
      TRUE ~ NA_real_
    )),
    # Worker bracket midpoint
    n_workers = case_when(
      workers == 1 ~ 1L,
      workers == 2 ~ 2L,
      workers == 3 ~ 3L,
      workers == 4 ~ 7L,
      workers == 5 ~ 15L,
      TRUE ~ NA_integer_
    )
  )

# Round 7 — Feb 2023 (peak crunch)
r7_raw <- read_csv(
  file.path(nlps, "p2r7_sect_a_2_5g_11b_13a_12.csv"),
  show_col_types = FALSE
)
r7 <- r7_raw |>
  select(hhid, zone, state, lga, sector,
         active_flag = s13a_respondent,
         sales_cat   = s13aq3,    # 1-9 relative scale (5=no change vs prior)
         workers     = s13aq4) |> # actual count
  mutate(
    round    = "R7",
    round_n  = 7L,
    active   = as.integer(!is.na(active_flag) & active_flag == 1),
    # R7 s13aq3 is a relative-change scale where values closer to 5 = stable
    # Use as ordinal; map roughly: 1-3=worse, 4=slightly worse, 5=same,
    # 6-7=better, 8-9=much better
    ln_sales = case_when(
      sales_cat %in% 1:2 ~ log(2500),
      sales_cat == 3     ~ log(12500),
      sales_cat == 4     ~ log(30000),
      sales_cat == 5     ~ log(60000),
      sales_cat == 6     ~ log(120000),
      sales_cat %in% 7:9 ~ log(300000),
      TRUE ~ NA_real_
    ),
    n_workers = as.integer(workers)
  )

# Round 11 — Apr 2024 (medium-run)
r11_raw <- read_csv(
  file.path(nlps, "p2r11_sect_a_6_6d_13b_12.csv"),
  show_col_types = FALSE
)
r11 <- r11_raw |>
  select(hhid, zone, state, lga, sector,
         active    = s13bq1,    # 1=operating, 2=not
         sales_cat = s13bq3) |> # 1-4 bracket
  mutate(
    round    = "R11",
    round_n  = 11L,
    active   = case_when(active == 1 ~ 1L, active == 2 ~ 0L, TRUE ~ NA_integer_),
    ln_sales = log(case_when(
      sales_cat == 1 ~ 2500,
      sales_cat == 2 ~ 12500,
      sales_cat == 3 ~ 60000,
      sales_cat == 4 ~ 300000,
      TRUE ~ NA_real_
    )),
    n_workers = NA_integer_
  )

# ── 3. Stack rounds ───────────────────────────────────────────────────────────
panel <- bind_rows(
  r5  |> select(hhid, zone, state, lga, sector, round, round_n,
                active, ln_sales, n_workers),
  r7  |> select(hhid, zone, state, lga, sector, round, round_n,
                active, ln_sales, n_workers),
  r11 |> select(hhid, zone, state, lga, sector, round, round_n,
                active, ln_sales, n_workers)
) |>
  mutate(
    round = factor(round, levels = c("R5","R7","R11")),
    post  = as.integer(round_n > 5),   # R7 and R11 are post-shock
    peak  = as.integer(round_n == 7),  # R7 = peak crunch
    med   = as.integer(round_n == 11)  # R11 = medium-run
  )

# ── 4. Restrict to enterprise households (active in R5) ──────────────────────
active_r5 <- r5 |> filter(active == 1) |> pull(hhid)
panel <- panel |> filter(hhid %in% active_r5)

cat("Enterprise sample (active in R5):", length(active_r5), "households\n")
cat("Stacked panel rows:", nrow(panel), "\n")

# ── 5. Merge fintech treatment ────────────────────────────────────────────────
panel <- panel |>
  left_join(ft |> rename(state = state_code), by = "state")

cat("Fintech matched:", mean(!is.na(panel$fintech_std)) * 100, "%\n")

# ── 6. Enterprise sector flag (retail = most cash-intensive) ─────────────────
# sector variable: 1=urban formal, 2=urban informal, 3=rural
# Use lga/zone for rural indicator; sector in R7 s13aq2 is industry code
# Here sector from geography: 1=urban, 2=rural in NLPS
panel <- panel |>
  mutate(
    rural = as.integer(sector == 2),
    # Firm size at baseline: workers in R5
    small_firm = NA_integer_
  )

# Merge R5 worker count for size classification
size_r5 <- r5 |>
  filter(active == 1) |>
  select(hhid, n_workers_r5 = n_workers)

panel <- panel |> left_join(size_r5, by = "hhid") |>
  mutate(small_firm = as.integer(!is.na(n_workers_r5) & n_workers_r5 <= 2))

# ── 7. Save ───────────────────────────────────────────────────────────────────
write_csv(panel, file.path(root, "data/processed/firm_panel_r5r7r11.csv"))
cat("\nSaved: data/processed/firm_panel_r5r7r11.csv\n")
cat("Unique households:", n_distinct(panel$hhid), "\n")
cat("Active in R7 (of R5 active):",
    sum(panel$active[panel$round == "R7"], na.rm = TRUE), "/",
    sum(panel$round == "R7"), "\n")
cat("Active in R11 (of R5 active):",
    sum(panel$active[panel$round == "R11"], na.rm = TRUE), "/",
    sum(panel$round == "R11"), "\n")
