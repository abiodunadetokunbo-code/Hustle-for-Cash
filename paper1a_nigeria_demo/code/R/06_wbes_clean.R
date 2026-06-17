# Script 06: Clean WBES Nigeria 2014 & 2025
# Paper 1a: Nigeria Demonetization / Cash Crunch
#
# Input : data/raw/wbes/Nigeria-2014-full-data.dta  (2,676 firms, 19 states)
#         data/raw/wbes/Nigeria-2025-full-data.dta  (1,043 firms, 6 zones)
#
# Output firm-level: wbes2014_firm.csv, wbes2025_firm.csv
# Output aggregates: wbes2014_state.csv (19 states), wbes2025_zone.csv (6 zones)
#
# Role in paper:
#   2014 state aggregates: pre-shock firm characteristics (cash dependence,
#     power-outage exposure, informality) for heterogeneity analysis
#   2025 zone aggregates: post-shock outcomes (2+ years post-demonetization)
#   Panel firms (panel == 1): potential firm-level DiD if linking key found

library(haven)
library(dplyr)
library(readr)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
wbes_dir <- file.path(root, "data/raw/wbes")
out_dir  <- file.path(root, "data/processed")
dir.create(out_dir, showWarnings = FALSE)

# ── 1. Load ───────────────────────────────────────────────────────────────────
cat("Loading WBES data...\n")
d14 <- read_dta(file.path(wbes_dir, "Nigeria-2014-full-data.dta"))
d25 <- read_dta(file.path(wbes_dir, "Nigeria-2025-full-data.dta"))
cat("2014: ", nrow(d14), "firms x", ncol(d14), "vars\n")
cat("2025: ", nrow(d25), "firms x", ncol(d25), "vars\n\n")

# ── Helper: recode WBES missing codes (-9=DK, -8=N/A, -7=refused) to NA ──────
wbes_num <- function(x) {
  x <- as.numeric(x)
  x[x %in% c(-9, -8, -7)] <- NA
  x
}

wbes_yn <- function(x) {
  # 1=Yes, 2=No in WBES coding → recode to 1/0
  x <- as.numeric(x)
  ifelse(x == 1, 1L, ifelse(x == 2, 0L, NA_integer_))
}

# ══════════════════════════════════════════════════════════════════════════════
# SECTION A: WBES 2014
# ══════════════════════════════════════════════════════════════════════════════
cat("=== Processing WBES 2014 ===\n")

firm14 <- d14 |>
  mutate(
    survey_year = 2014L,
    idstd       = as.character(idstd),

    # Geography: a2 value labels = state name (19 states sampled)
    state       = as.character(as_factor(a2)),

    # Firm characteristics
    sector      = as.character(as_factor(a4b)),
    size_cat    = as.character(as_factor(a6a)),

    # Outcome: total annual sales (FY 2013, LCU)
    sales       = wbes_num(d2),

    # Employment
    emp_perm    = wbes_num(l1),
    emp_temp    = wbes_num(l6),
    emp_total   = pmax(wbes_num(l1) + wbes_num(l6), wbes_num(l1), na.rm = TRUE),

    # Finance access variables
    pct_own_wc   = wbes_num(k3a),     # % WC from own funds
    pct_bank_wc  = wbes_num(k3bc),    # % WC from bank borrowing
    has_bank_acct = wbes_yn(k6),      # checking/savings account
    has_credit    = wbes_yn(k8),      # active loan or line of credit
    fin_obstacle  = case_when(
      as.numeric(k30) %in% 5:6 ~ 1L, # Major or Very Severe obstacle
      as.numeric(k30) %in% 1:4 ~ 0L,
      TRUE ~ NA_integer_
    ),

    # Finance-exclusion cash-dependence index (0-3, normalized to 0-1):
    #   k6==No → no bank account (must transact in cash)
    #   k3bc==0 → no WC financing from banks
    #   k30 >= Major → perceives finance as major constraint
    # Unlike k3a alone, this is not confounded by profitable self-funding.
    cash_dep = {
      s1 <- as.integer(wbes_yn(k6) == 0)   # unbanked
      s2 <- as.integer(wbes_num(k3bc) == 0 | is.na(wbes_num(k3bc)))
      s3 <- fin_obstacle
      rowMeans(cbind(s1, s2, s3), na.rm = TRUE)
    },

    # Power: outages
    outage_yn  = wbes_yn(c6),
    outages_pm = wbes_num(c7),
    outage_hrs = wbes_num(c8a),

    # Informality: % sales as informal payments
    pct_bribe  = wbes_num(j7a)
  ) |>
  select(
    idstd, survey_year, state, sector, size_cat,
    sales, emp_perm, emp_temp, emp_total,
    pct_own_wc, pct_bank_wc, has_bank_acct, has_credit,
    fin_obstacle, cash_dep,
    outage_yn, outages_pm, outage_hrs, pct_bribe
  )

cat("2014 firm-level rows:", nrow(firm14), "\n")
cat("States:", paste(sort(unique(firm14$state)), collapse = ", "), "\n\n")

write_csv(firm14, file.path(out_dir, "wbes2014_firm.csv"))
cat("Saved: data/processed/wbes2014_firm.csv\n\n")

# ── State-level aggregates ────────────────────────────────────────────────────
state14 <- firm14 |>
  group_by(state) |>
  summarise(
    n_firms          = n(),
    # Finance exclusion components
    pct_unbanked     = mean(has_bank_acct == 0, na.rm = TRUE),
    pct_no_credit    = mean(has_credit == 0,    na.rm = TRUE),
    pct_fin_obstacle = mean(fin_obstacle == 1,  na.rm = TRUE),
    # Composite cash-dependence index (avg of 3 exclusion indicators)
    mean_cash_dep    = mean(cash_dep, na.rm = TRUE),
    # WC source breakdown
    mean_own_wc      = mean(pct_own_wc,  na.rm = TRUE),
    mean_bank_wc     = mean(pct_bank_wc, na.rm = TRUE),
    # Power
    pct_outage       = mean(outage_yn,   na.rm = TRUE),
    mean_outages_pm  = mean(outages_pm,  na.rm = TRUE),
    # Size and informality
    mean_emp         = mean(emp_total,   na.rm = TRUE),
    pct_bribe        = mean(pct_bribe > 0, na.rm = TRUE),
    .groups          = "drop"
  )

write_csv(state14, file.path(out_dir, "wbes2014_state.csv"))
cat("Saved: data/processed/wbes2014_state.csv\n")
cat("States:", nrow(state14), "\n")
print(state14)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION B: WBES 2025
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== Processing WBES 2025 ===\n")

firm25 <- d25 |>
  mutate(
    survey_year = 2025L,
    idstd       = as.character(idstd),
    is_panel    = as.integer(panel == 1),

    # Geography: a2 = geopolitical zone (6 zones)
    zone        = as.character(as_factor(a2)),

    # Firm characteristics
    sector      = as.character(as_factor(a4a)),
    size_cat    = as.character(as_factor(a6a)),

    # Employment (post-shock)
    emp_perm    = wbes_num(l1),

    # Finance: WC source breakdown (k6/k30 not in 2025 — use WC shares)
    pct_own_wc      = wbes_num(k3a),
    pct_bank_wc     = wbes_num(k3bc),
    pct_informal_wc = wbes_num(k3dgh),

    # Finance-exclusion cash-dependence (comparable to 2014 where possible):
    #   no bank WC financing + any informal WC financing
    cash_dep = {
      s1 <- as.integer(
        is.na(wbes_num(k3bc)) | wbes_num(k3bc) == 0
      )
      s2 <- as.integer(
        !is.na(wbes_num(k3dgh)) & wbes_num(k3dgh) > 0
      )
      rowMeans(cbind(s1, s2), na.rm = TRUE)
    },

    # Power outages (post-shock)
    outage_yn   = wbes_yn(c6),
    outages_pm  = wbes_num(c7)
  ) |>
  select(
    idstd, survey_year, is_panel, zone, sector, size_cat,
    emp_perm, pct_own_wc, pct_bank_wc, pct_informal_wc,
    cash_dep, outage_yn, outages_pm
  )

cat("2025 firm-level rows:", nrow(firm25), "\n")
cat("Panel firms (is_panel=1):", sum(firm25$is_panel), "\n")
cat("Zones:", paste(sort(unique(firm25$zone)), collapse=", "), "\n\n")

write_csv(firm25, file.path(out_dir, "wbes2025_firm.csv"))
cat("Saved: data/processed/wbes2025_firm.csv\n\n")

# ── Zone-level aggregates ─────────────────────────────────────────────────────
zone25 <- firm25 |>
  group_by(zone) |>
  summarise(
    n_firms          = n(),
    n_panel          = sum(is_panel),
    pct_outage       = mean(outage_yn, na.rm = TRUE),
    mean_outages_pm  = mean(outages_pm, na.rm = TRUE),
    mean_cash_dep    = mean(cash_dep, na.rm = TRUE),
    pct_high_cashdep = mean(cash_dep > 0.8, na.rm = TRUE),
    mean_emp         = mean(emp_perm, na.rm = TRUE),
    .groups          = "drop"
  )

write_csv(zone25, file.path(out_dir, "wbes2025_zone.csv"))
cat("Saved: data/processed/wbes2025_zone.csv\n")
print(zone25)

# ══════════════════════════════════════════════════════════════════════════════
# SECTION C: Cross-wave comparison (2014 vs 2025, zone-level)
# ══════════════════════════════════════════════════════════════════════════════
cat("\n=== Cross-wave: power outages and cash dependence ===\n")

# Map 2014 states to geopolitical zones for comparison
state_to_zone <- c(
  "Abia"        = "South East",  "Anambra"     = "South East",
  "Enugu"       = "South East",  "Imo"         = "South East",
  "Ebonyi"      = "South East",
  "Lagos"       = "South West",  "Oyo"         = "South West",
  "Ogun"        = "South West",  "Osun"        = "South West",
  "Ondo"        = "South West",  "Ekiti"      = "South West",
  "Rivers"      = "South South", "Delta"      = "South South",
  "Bayelsa"     = "South South", "Cross river" = "South South",
  "Akwa Ibom"   = "South South", "Edo"         = "South South",
  "Kano"        = "North West",  "Kaduna"      = "North West",
  "Sokoto"      = "North West",  "Zamfara"     = "North West",
  "Kebbi"       = "North West",  "Katsina"     = "North West",
  "Jigawa"      = "North West",
  "Abuja"       = "North Central","Kwara"      = "North Central",
  "Nasarawa"    = "North Central","Niger"      = "North Central",
  "Benue"       = "North Central","Kogi"       = "North Central",
  "Plateau"     = "North Central",
  "Gombe"       = "North East",  "Borno"       = "North East",
  "Yobe"        = "North East",  "Adamawa"     = "North East",
  "Taraba"      = "North East",  "Bauchi"      = "North East"
)

zone14 <- firm14 |>
  mutate(zone = state_to_zone[state]) |>
  filter(!is.na(zone)) |>
  group_by(zone) |>
  summarise(
    pct_outage_2014   = mean(outage_yn, na.rm = TRUE),
    mean_cashdep_2014 = mean(cash_dep, na.rm = TRUE),
    .groups = "drop"
  )

compare <- zone25 |>
  select(zone, pct_outage_2025 = pct_outage,
         mean_cashdep_2025 = mean_cash_dep) |>
  left_join(zone14, by = "zone") |>
  mutate(
    d_outage  = pct_outage_2025 - pct_outage_2014,
    d_cashdep = mean_cashdep_2025 - mean_cashdep_2014
  ) |>
  arrange(zone)

cat("\nChange in power outages and cash dependence (2014 → 2025):\n")
print(compare)
cat("\nPositive d_cashdep = more self-financed post-shock\n")
cat("(credit-market retrenchment / persistent informality after demonetization)\n")

cat("\nScript 06 complete.\n")
