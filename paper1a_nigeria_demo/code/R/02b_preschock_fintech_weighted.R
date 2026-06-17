# Script 02b: Pre-Shock Fintech Index — Population-Weighted Rebuild
# Replaces 02_preschock_fintech_proxy.R output with properly weighted estimates.
#
# Problem in original: unweighted means give artificially high fintech scores
# to small-sample northern states (Taraba n=32, Jigawa n=26) whose few
# phone-survey respondents are highly selected tech-savvy households.
# Fix: use wt_p2round6 (NLPS expansion weights) so each household is weighted
# by the number of population households it represents.

library(readr); library(dplyr); library(tidyr)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")
out  <- file.path(root, "data/instruments/preschock_fintech_state.csv")

# ── 1. Load R6 financial services and geography ───────────────────────────────
s5 <- read_csv(file.path(nlps, "p2r6_sect_5.csv"), show_col_types = FALSE)
sa <- read_csv(
  file.path(nlps, "p2r6_sect_a_2_5_6_8_11b_12.csv"),
  show_col_types = FALSE
) |>
  select(hhid, zone, state, lga, sector,
         weight = wt_p2round6) |>
  filter(!is.na(weight))

cat("R6 households with valid weights:", nrow(sa), "\n")
cat("States:", n_distinct(sa$state), "\n")

# ── 2. Service binary indicators ──────────────────────────────────────────────
service_labels <- c(
  "1" = "pension_insurance", "2" = "securities",
  "3" = "mobile_money",      "4" = "microfinance_coop",
  "5" = "bank_account",      "6" = "mobile_phone_ussd",
  "7" = "mobile_banking_app","8" = "credit_card"
)

hh_wide <- s5 |>
  mutate(
    uses      = as.integer(s5fq4 == 1),
    svc_label = service_labels[as.character(service_cd)]
  ) |>
  filter(!is.na(svc_label)) |>
  select(hhid, svc_label, uses) |>
  pivot_wider(names_from = svc_label, values_from = uses, values_fill = 0)

# Fintech index: mean of four digital-payment services (codes 3,5,6,7)
fintech_cols <- intersect(
  c("mobile_money","bank_account","mobile_phone_ussd","mobile_banking_app"),
  names(hh_wide)
)
hh_wide <- hh_wide |>
  mutate(
    fintech_index   = rowMeans(across(all_of(fintech_cols)), na.rm = TRUE),
    any_digital_pay = as.integer(fintech_index > 0)
  )

# ── 3. Merge weights and aggregate with weighted.mean ─────────────────────────
hh_merged <- hh_wide |>
  inner_join(sa |> select(hhid, state, zone, sector, weight), by = "hhid")

cat("Matched households:", nrow(hh_merged), "\n\n")

all_svc_cols <- c(unname(service_labels), "fintech_index", "any_digital_pay")
present_cols <- intersect(all_svc_cols, names(hh_merged))

state_df <- hh_merged |>
  group_by(state) |>
  summarise(
    n_hh = n(),
    sum_weight = sum(weight),
    across(
      all_of(present_cols),
      ~ round(weighted.mean(.x, w = weight, na.rm = TRUE) * 100, 2),
      .names = "pct_{.col}"
    ),
    .groups = "drop"
  ) |>
  rename(state_code = state)

# ── 4. Compare weighted vs unweighted ─────────────────────────────────────────
orig <- read_csv(out, show_col_types = FALSE)

cat("=== Top 10 states: weighted vs unweighted fintech_index ===\n")
compare <- orig |>
  select(state_code, n_hh, unweighted = pct_fintech_index) |>
  left_join(state_df |> select(state_code, weighted = pct_fintech_index),
            by = "state_code") |>
  arrange(desc(weighted))

print(compare |> head(10))

cat("\n=== Correlation: weighted vs unweighted ===\n")
cat("r =", round(cor(compare$weighted, compare$unweighted, use="complete"), 3), "\n")

cat("\n=== Bottom 10 by weighted (truly low fintech) ===\n")
print(compare |> arrange(weighted) |> head(10))

# ── 5. Save ───────────────────────────────────────────────────────────────────
write_csv(state_df, out)
cat("\nOverwrote:", out, "\n")
cat("States saved:", nrow(state_df), "\n")
cat("Script 02b complete.\n")
