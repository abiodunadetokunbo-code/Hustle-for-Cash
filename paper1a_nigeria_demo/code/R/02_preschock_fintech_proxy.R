# Script 02: Pre-Shock Fintech Density Proxy from NLPS Round 6
# Paper 1a: Nigeria Demonetization
#
# Constructs state-level digital financial services penetration as of
# October 15, 2022 — 11 days before the CBN announcement.
# Uses NLPS Round 6 sect_5 (financial services module).
#
# Requires: dplyr, readr (both already installed)
# Run: Rscript code/R/02_preschock_fintech_proxy.R
# Output: data/instruments/preschock_fintech_state.csv

library(dplyr)
library(readr)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")
out  <- file.path(root, "data/instruments/preschock_fintech_state.csv")

# ── 1. Load Round 6 data ──────────────────────────────────────────────────────
# Round 6 interview date: October 15, 2022 (11 days pre-announcement)
cat("Loading NLPS Round 6 (Oct 15, 2022)...\n")

s5 <- read_csv(file.path(nlps, "p2r6_sect_5.csv"), show_col_types = FALSE)
sa <- read_csv(file.path(nlps, "p2r6_sect_a_2_5_6_8_11b_12.csv"),
               show_col_types = FALSE) |>
  select(hhid, zone, state, lga, sector, any_of("wt_p2round6"))

cat("  Households in sect_5:", n_distinct(s5$hhid), "\n")
cat("  States covered:", n_distinct(sa$state), "\n")

# ── 2. Service code penetration rates ─────────────────────────────────────────
# s5fq4 = 1 (YES) / 2 (NO) for each service code 1-8
# Penetration rates in R6 (Oct 2022):
#   Code 1:  0.7%  → pension / formal insurance
#   Code 2:  0.0%  → securities / investment
#   Code 3:  5.9%  → mobile money (OPay, PalmPay, MTN MoMo)
#   Code 4:  3.4%  → microfinance bank / cooperative
#   Code 5: 14.7%  → formal bank account (commercial bank)
#   Code 6: 86.2%  → mobile phone / USSD (*737#, *901# etc.)
#   Code 7:  5.1%  → mobile banking app / internet banking
#   Code 8:  0.0%  → credit card

service_labels <- c(
  "1" = "pension_insurance",
  "2" = "securities",
  "3" = "mobile_money",
  "4" = "microfinance_coop",
  "5" = "bank_account",
  "6" = "mobile_phone_ussd",
  "7" = "mobile_banking_app",
  "8" = "credit_card"
)

# ── 3. Pivot to household-level wide format ───────────────────────────────────
cat("Building household-level fintech profile...\n")

hh_wide <- s5 |>
  mutate(
    uses       = as.integer(s5fq4 == 1),
    svc_label  = service_labels[as.character(service_cd)]
  ) |>
  filter(!is.na(svc_label)) |>
  select(hhid, svc_label, uses) |>
  tidyr::pivot_wider(
    names_from  = svc_label,
    values_from = uses,
    values_fill = 0
  )

# Composite fintech readiness index: mobile money + bank account + USSD + app
fintech_cols <- intersect(
  c("mobile_money", "bank_account", "mobile_phone_ussd", "mobile_banking_app"),
  names(hh_wide)
)
hh_wide <- hh_wide |>
  mutate(
    fintech_index     = rowSums(across(all_of(fintech_cols)), na.rm = TRUE),
    any_digital_pay   = as.integer(fintech_index > 0)
  )

# ── 4. Merge with state geography ─────────────────────────────────────────────
hh_merged <- hh_wide |>
  inner_join(sa |> select(hhid, state, zone, sector), by = "hhid")

# ── 5. Aggregate to state level ───────────────────────────────────────────────
cat("Aggregating to state level...\n")

# All numeric service columns
all_svc_cols <- c(unname(service_labels), "fintech_index", "any_digital_pay")
present_cols <- intersect(all_svc_cols, names(hh_merged))

state_df <- hh_merged |>
  group_by(state) |>
  summarise(
    n_hh = n(),
    across(
      all_of(present_cols),
      ~ round(mean(.x, na.rm = TRUE) * 100, 2),
      .names = "pct_{.col}"
    ),
    .groups = "drop"
  ) |>
  rename(state_code = state)

# ── 6. Save ───────────────────────────────────────────────────────────────────
write_csv(state_df, out)
cat("\nSaved:", out, "\n")
cat("States:", nrow(state_df), "\n")

# ── 7. Report ─────────────────────────────────────────────────────────────────
cat("\n--- Pre-shock fintech penetration by state (Oct 2022) ---\n")
cat("Variable definitions:\n")
cat("  pct_mobile_money      : % households with mobile money account (OPay etc.)\n")
cat("  pct_bank_account      : % households with formal bank account\n")
cat("  pct_mobile_phone_ussd : % households using mobile/USSD banking\n")
cat("  pct_fintech_index     : mean digital services count per household x100\n")

key_cols <- c("state_code", "n_hh",
              intersect(c("pct_mobile_money","pct_bank_account",
                          "pct_mobile_phone_ussd","pct_fintech_index"),
                        names(state_df)))

cat("\nTop 10 states — highest fintech readiness (most insulated from cash crunch):\n")
state_df |>
  arrange(desc(pct_fintech_index)) |>
  slice_head(n = 10) |>
  select(all_of(key_cols)) |>
  print(n = 10)

cat("\nBottom 10 states — lowest fintech readiness (most exposed to cash crunch):\n")
state_df |>
  arrange(pct_fintech_index) |>
  slice_head(n = 10) |>
  select(all_of(key_cols)) |>
  print(n = 10)

cat("\nScript complete.\n")
