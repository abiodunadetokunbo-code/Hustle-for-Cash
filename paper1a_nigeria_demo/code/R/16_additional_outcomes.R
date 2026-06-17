# Script 16: Additional Household-Level Outcomes
# (A) Wage employment panel: R5 → R11 (same DiD as firm panel)
# (B) Food insecurity cross-section at R7 (peak crunch)
# Both outcomes use the same Fintech_s treatment variable as the firm panel.
# Outputs: paper/tables/tab_additional_outcomes.tex

library(readr); library(dplyr); library(fixest)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps    <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")
tab_dir <- file.path(root, "paper/tables")
dir.create(tab_dir, showWarnings = FALSE)

# ── 0. Load fintech treatment ─────────────────────────────────────────────────
ft <- read_csv(
  file.path(root, "data/instruments/preschock_fintech_state.csv"),
  show_col_types = FALSE
) |>
  mutate(fintech_std = as.numeric(scale(pct_fintech_index))) |>
  select(state_code, fintech_std, pct_fintech_index)

# ── A. Wage employment panel (R5 + R11) ───────────────────────────────────────
# s6q1: employment status
#   1 = has paid job / wage employment
#   2 = not employed (no paid job)
#   3 = looking / unemployed
# Binary: employed = (s6q1 == 1)

read_emp <- function(file, round_label) {
  d <- read_csv(file, show_col_types = FALSE)
  d |>
    select(hhid, zone, state, lga, sector,
           any_of(c("wt_p2round5","wt_p2round11")),
           s6q1 = matches("^s6q1$")) |>
    mutate(
      round    = round_label,
      employed = as.integer(s6q1 == 1),
      rural    = as.integer(sector == 2)
    ) |>
    select(hhid, state, rural, round, employed)
}

r5_emp  <- read_emp(file.path(nlps, "p2r5_sect_a_2_5_6_9a_11b_13_12.csv"),  "R5")
r11_emp <- read_emp(file.path(nlps, "p2r11_sect_a_6_6d_13b_12.csv"), "R11")

cat("R5 employment: n=", nrow(r5_emp),
    "| employed rate:", round(mean(r5_emp$employed, na.rm=TRUE),3), "\n")
cat("R11 employment: n=", nrow(r11_emp),
    "| employed rate:", round(mean(r11_emp$employed, na.rm=TRUE),3), "\n")

# Keep households observed in BOTH rounds
hh_both <- intersect(r5_emp$hhid, r11_emp$hhid)
cat("Households in both R5 and R11:", length(hh_both), "\n")

emp_panel <- bind_rows(
  r5_emp  |> filter(hhid %in% hh_both),
  r11_emp |> filter(hhid %in% hh_both)
) |>
  mutate(round = factor(round, levels = c("R5","R11"))) |>
  left_join(ft |> rename(state = state_code), by = "state") |>
  mutate(
    hh_f    = factor(hhid),
    state_f = factor(state)
  )

cat("Employment panel: ", nrow(emp_panel), "rows |",
    n_distinct(emp_panel$hhid), "households |",
    n_distinct(emp_panel$state_f), "state clusters\n")

# Main employment DiD
m_emp1 <- feols(
  employed ~ i(round, fintech_std, ref = "R5") | hh_f + round,
  data = emp_panel, cluster = ~state_f
)
m_emp2 <- feols(
  employed ~ i(round, fintech_std, ref = "R5") + rural | hh_f + round,
  data = emp_panel, cluster = ~state_f
)

cat("\nEmployment DiD:\n")
cat("  R11 coef:", round(coef(m_emp1)["round::R11:fintech_std"], 4),
    "| p:", round(pvalue(m_emp1)["round::R11:fintech_std"], 3), "\n")

# ── B. Food insecurity at R7 (peak crunch) ────────────────────────────────────
# s5gq0: filter (1 = answered food module, 2 = skipped)
# s5gq1: "In the past month, did you or your household worry about not having
#          enough food to eat?" (1=yes, 2=no, NA=not applicable)
# s5gq5: "In the past month, did your household run out of food?" (1=yes)
# Outcome: any food insecurity = s5gq1==1 OR s5gq5==1

r7_food <- read_csv(
  file.path(nlps, "p2r7_sect_a_2_5g_11b_13a_12.csv"),
  show_col_types = FALSE
) |>
  select(hhid, state, zone, lga, sector,
         s5gq0, s5gq1, s5gq3, s5gq4, s5gq5) |>
  mutate(
    food_worry   = as.integer(!is.na(s5gq1) & s5gq1 == 1),
    food_runout  = as.integer(!is.na(s5gq5) & s5gq5 == 1),
    food_insecure = as.integer(food_worry == 1 | food_runout == 1),
    rural = as.integer(sector == 2)
  ) |>
  filter(!is.na(s5gq0)) |>          # restrict to those who answered filter
  left_join(ft |> rename(state = state_code), by = "state") |>
  mutate(state_f = factor(state))

cat("\nFood insecurity at R7 (peak crunch):\n")
cat("  Observations:", nrow(r7_food), "\n")
cat("  Food worry rate:", round(mean(r7_food$food_worry, na.rm=TRUE), 3), "\n")
cat("  Food runout rate:", round(mean(r7_food$food_runout, na.rm=TRUE), 3), "\n")
cat("  Any food insecure:", round(mean(r7_food$food_insecure, na.rm=TRUE), 3), "\n")

# Cross-sectional OLS at R7.
# Treatment (fintech_std) is state-level: state FE would absorb it entirely.
# Instead: include zone dummies (6 zones) to absorb broad regional differences,
# plus household-level rural indicator. Cluster SEs at state level.
r7_food <- r7_food |> mutate(zone_f = factor(zone))

m_food1 <- feols(
  food_insecure ~ fintech_std | zone_f,
  data = r7_food, cluster = ~state_f
)
m_food2 <- feols(
  food_insecure ~ fintech_std + rural | zone_f,
  data = r7_food, cluster = ~state_f
)
m_food3 <- feols(
  food_worry ~ fintech_std + rural | zone_f,
  data = r7_food, cluster = ~state_f
)
m_food4 <- feols(
  food_runout ~ fintech_std + rural | zone_f,
  data = r7_food, cluster = ~state_f
)

cat("\nFood insecurity cross-section (R7, state FE):\n")
cat("  Any insecure ~ fintech:", round(coef(m_food1)["fintech_std"], 4),
    "| p:", round(pvalue(m_food1)["fintech_std"], 3), "\n")
cat("  Food worry   ~ fintech:", round(coef(m_food3)["fintech_std"], 4),
    "| p:", round(pvalue(m_food3)["fintech_std"], 3), "\n")
cat("  Food runout  ~ fintech:", round(coef(m_food4)["fintech_std"], 4),
    "| p:", round(pvalue(m_food4)["fintech_std"], 3), "\n")

# ── C. Export combined table ──────────────────────────────────────────────────
dict_add <- c(
  "round::R11:fintech_std" = "Fintech $\\times$ R11 (medium run)",
  "fintech_std"            = "Fintech index (std.)",
  "rural"                  = "Rural"
)

etable(
  m_emp1, m_food1, m_food2, m_food3, m_food4,
  title   = "Additional Household Outcomes: Wage Employment and Food Insecurity",
  headers = list(
    "^" = .("Employed","Food insecure","Food insecure","Food worry","Food runout")
  ),
  keep = c("%round::R11", "%fintech_std"),
  dict = dict_add,
  signif.code = c("***" = .01, "**" = .05, "*" = .10),
  extralines = list(
    "Sample"       = c("Panel R5--R11","R7 peak","R7 peak","R7 peak","R7 peak"),
    "Household FE" = c("\\checkmark","---","---","---","---"),
    "Zone FE"      = c("---","\\checkmark","\\checkmark","\\checkmark","\\checkmark"),
    "Round FE"     = c("\\checkmark","---","---","---","---"),
    "Rural ctrl."  = c("No","No","Yes","Yes","Yes")
  ),
  notes = paste(
    "Col (1): household wage-employment DiD (R5 Aug 2022 to R11 Apr 2024) with household FE.",
    "The negative coefficient indicates high-fintech states saw larger declines in",
    "formal wage employment, consistent with a composition shift toward self-employment",
    "in surviving enterprises (documented in Table~\\ref{tab:firm_did}).",
    "Cols (2)--(5): food insecurity cross-section at R7 (peak crunch, Feb 2023).",
    "Treatment is state-level fintech index; zone FE (6 zones) absorb broad regional",
    "differences; state FE cannot be included as they are collinear with state-level treatment.",
    "Food insecure = 1 if household worried about or ran out of food in past month (FIES items).",
    "Cross-sections are descriptive; no household pre-period baseline is available.",
    "Standard errors clustered at the state level."
  ),
  file    = file.path(tab_dir, "tab_additional_outcomes.tex"),
  tex     = TRUE,
  replace = TRUE
)
cat("Saved: tab_additional_outcomes.tex\n")

# ── D. Save key numbers ───────────────────────────────────────────────────────
saveRDS(
  list(
    emp_r11_coef   = coef(m_emp1)["round::R11:fintech_std"],
    emp_r11_p      = pvalue(m_emp1)["round::R11:fintech_std"],
    food_ins_coef  = coef(m_food1)["fintech_std"],
    food_ins_p     = pvalue(m_food1)["fintech_std"],
    food_worry_coef = coef(m_food3)["fintech_std"],
    food_worry_p    = pvalue(m_food3)["fintech_std"],
    food_runout_coef = coef(m_food4)["fintech_std"],
    food_runout_p    = pvalue(m_food4)["fintech_std"],
    n_emp_panel    = n_distinct(emp_panel$hhid),
    n_food_r7      = nrow(r7_food),
    food_insecure_rate = mean(r7_food$food_insecure, na.rm=TRUE)
  ),
  file.path(root, "data/processed/key_numbers_additional.rds")
)

cat("\n=== Additional outcomes summary ===\n")
cat(sprintf("Employment R11: coef=%.4f  p=%.3f  (N=%d HH)\n",
            coef(m_emp1)["round::R11:fintech_std"],
            pvalue(m_emp1)["round::R11:fintech_std"],
            n_distinct(emp_panel$hhid)))
cat(sprintf("Food insecure (R7 cross-section): coef=%.4f  p=%.3f  (N=%d)\n",
            coef(m_food1)["fintech_std"],
            pvalue(m_food1)["fintech_std"],
            nrow(r7_food)))
cat("Script 16 complete.\n")
