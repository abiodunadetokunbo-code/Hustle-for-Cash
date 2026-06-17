# Script 11: Firm-level DiD (NLPS) + Heterogeneity + Balance + Attrition + Welfare
# Outputs: tab_firm_did.tex, tab_heterogeneity.tex,
#          atab_balance.tex, atab_attrition.tex, atab_sales_decomp.tex

library(readr); library(dplyr); library(fixest); library(tidyr)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps    <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")
tab_dir <- file.path(root, "paper/tables")
dir.create(tab_dir, showWarnings = FALSE)

# ── 1. Load firm panel ────────────────────────────────────────────────────────
fp <- read_csv(
  file.path(root, "data/processed/firm_panel_r5r7r11.csv"),
  show_col_types = FALSE
) |>
  mutate(
    round   = factor(round, levels = c("R5","R7","R11")),
    state_f = factor(state),
    hh_f    = factor(hhid),
    fintech_tercile = ntile(fintech_std, 3)
  )

cat("Firm panel rows:", nrow(fp),
    "| Unique HHs:", n_distinct(fp$hhid), "\n")
cat("Active rates: R5=", mean(fp$active[fp$round=="R5"], na.rm=TRUE),
    "R7=",  mean(fp$active[fp$round=="R7"],  na.rm=TRUE),
    "R11=", mean(fp$active[fp$round=="R11"], na.rm=TRUE), "\n")

# ── 2. Main firm DiD ──────────────────────────────────────────────────────────
m_act1 <- feols(
  active ~ i(round, fintech_std, ref = "R5") | hh_f + round,
  data = fp, cluster = ~state_f
)
m_act2 <- feols(
  active ~ i(round, fintech_std, ref = "R5") + rural | hh_f + round,
  data = fp, cluster = ~state_f
)

fp_sales <- fp |> filter(!is.na(ln_sales))
m_sal1 <- feols(
  ln_sales ~ i(round, fintech_std, ref = "R5") | hh_f + round,
  data = fp_sales, cluster = ~state_f
)
m_sal2 <- feols(
  ln_sales ~ i(round, fintech_std, ref = "R5") + rural | hh_f + round,
  data = fp_sales, cluster = ~state_f
)

fp_work <- fp |> filter(!is.na(n_workers) & round %in% c("R5","R7")) |>
  mutate(round = factor(round, levels = c("R5","R7")))
m_wk1 <- feols(
  n_workers ~ i(round, fintech_std, ref = "R5") | hh_f + round,
  data = fp_work, cluster = ~state_f
)

cat("\n=== Main firm DiD ===\n")
cat("Active | R7 coef:", round(coef(m_act1)["round::R7:fintech_std"],4),
    "p:", round(pvalue(m_act1)["round::R7:fintech_std"],3), "\n")
cat("Active | R11 coef:", round(coef(m_act1)["round::R11:fintech_std"],4),
    "p:", round(pvalue(m_act1)["round::R11:fintech_std"],3), "\n")
cat("Sales  | R7 coef:", round(coef(m_sal1)["round::R7:fintech_std"],4),
    "p:", round(pvalue(m_sal1)["round::R7:fintech_std"],3), "\n")
cat("Workers| R7 coef:", round(coef(m_wk1)["round::R7:fintech_std"],4),
    "p:", round(pvalue(m_wk1)["round::R7:fintech_std"],3), "\n")

dict_firm <- c(
  "round::R7:fintech_std"  = "Fintech $\\times$ R7 (peak crunch)",
  "round::R11:fintech_std" = "Fintech $\\times$ R11 (medium run)",
  "rural" = "Rural"
)

etable(
  m_act1, m_act2, m_sal1, m_sal2, m_wk1,
  title   = "Firm-Level Effects of the Naira Crunch",
  headers = list("^" = .("Active","Active","Log sales","Log sales","Workers")),
  keep    = "%round::",
  dict    = dict_firm,
  signif.code = c("***"=.01,"**"=.05,"*"=.10),
  extralines = list(
    "Enterprise FE" = rep("\\checkmark", 5),
    "Round FE"      = rep("\\checkmark", 5),
    "Rural control" = c("No","Yes","No","Yes","No")
  ),
  notes = paste(
    "Sample: non-farm household enterprises active in NLPS Round 5",
    "(August 2022 pre-shock baseline). Round 5 is the omitted baseline.",
    "Fintech is the standardised state-level digital payment index from",
    "NLPS Round 6 (October 2022). Workers only available for R5 and R7.",
    "Standard errors clustered at the state level.",
    "Inference robustness: see Appendix Table A3."
  ),
  file    = file.path(tab_dir, "tab_firm_did.tex"),
  tex     = TRUE, replace = TRUE
)
cat("Saved: tab_firm_did.tex\n")

# ── 3. Heterogeneity ──────────────────────────────────────────────────────────
run_het <- function(dat)
  feols(active ~ i(round, fintech_std, ref="R5") | hh_f + round,
        data = dat, cluster = ~state_f)

m_rural <- run_het(fp |> filter(!is.na(rural) & rural == 1))
m_urban <- run_het(fp |> filter(!is.na(rural) & rural == 0))
m_small <- run_het(fp |> filter(!is.na(small_firm) & small_firm == 1))
m_large <- run_het(fp |> filter(!is.na(small_firm) & small_firm == 0))

etable(
  m_rural, m_urban, m_small, m_large,
  title   = "Heterogeneity by Location and Firm Size",
  headers = list("^" = .("Rural","Urban","Small","Large")),
  keep    = "%round::",
  dict    = dict_firm,
  signif.code = c("***"=.01,"**"=.05,"*"=.10),
  notes = paste("Dependent variable: enterprise active (binary).",
                "Split-sample estimates. Small = below-median workers at R5 baseline.",
                "Standard errors clustered at state level."),
  file    = file.path(tab_dir, "tab_heterogeneity.tex"),
  tex     = TRUE, replace = TRUE
)
cat("Saved: tab_heterogeneity.tex\n")

# ── 4. Baseline balance ────────────────────────────────────────────────────────
# At R5, regress baseline enterprise characteristics on fintech_std
# (no FE — pure cross-section). If fintech is predetermined, these should be zero.
fp_r5 <- fp |> filter(round == "R5")

bal_vars <- list(
  "Log weekly sales (R5)"     = "ln_sales",
  "Enterprise workers (R5)"   = "n_workers_r5",
  "Rural enterprise"          = "rural",
  "Small firm"                = "small_firm"
)

bal_results <- lapply(names(bal_vars), function(label) {
  varname <- bal_vars[[label]]
  if (!varname %in% names(fp_r5)) return(NULL)
  d <- fp_r5 |> filter(!is.na(.data[[varname]]))
  m <- feols(as.formula(paste(varname, "~ fintech_std")),
             data = d, cluster = ~state_f)
  data.frame(
    variable = label,
    coef     = round(coef(m)["fintech_std"], 4),
    se       = round(se(m)["fintech_std"], 4),
    p        = round(pvalue(m)["fintech_std"], 3),
    n        = nrow(d),
    stringsAsFactors = FALSE
  )
})
bal_df <- do.call(rbind, Filter(Negate(is.null), bal_results))

cat("\n=== Baseline balance ===\n")
print(bal_df)

star_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}

bal_tex <- c(
  "\\begin{table}[H]",
  "\\begin{threeparttable}",
  "\\caption{Baseline Balance: Fintech and Pre-Shock Enterprise Characteristics}",
  "\\label{atab:balance}",
  "\\small\\begin{tabular}{lcccc}",
  "\\toprule",
  "Baseline characteristic & Coef. & SE & $p$-value & $N$ \\\\",
  "\\midrule",
  sapply(seq_len(nrow(bal_df)), function(i) {
    r <- bal_df[i,]
    sprintf("%s & %.4f & %.4f & %.3f%s & %d \\\\",
            r$variable, r$coef, r$se, r$p, star_fn(r$p), r$n)
  }),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Cross-sectional OLS at Round 5 (Aug 2022) baseline.",
    " Dependent variable is the listed enterprise characteristic.",
    " Regressor is the standardised state-level fintech index ($\\textit{Fintech}_s$)",
    " measured in NLPS Round 6 (October 2022, 11 days before the CBN announcement).",
    " No fixed effects; standard errors clustered at the state level.",
    " Coefficients close to zero and statistically insignificant confirm that",
    " fintech adoption is uncorrelated with pre-shock enterprise characteristics,",
    " supporting the parallel-trends assumption."
  ),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
writeLines(bal_tex, file.path(tab_dir, "atab_balance.tex"))
cat("Saved: atab_balance.tex\n")

# ── 5. Attrition: pre-R5 rounds ───────────────────────────────────────────────
# Load R3 (Aug 2021, 2 rounds before R5) to identify households that had
# enterprises earlier but are NOT in the R5 active sample.
r3_path <- file.path(nlps, "p2r3_sect_a_2_5_6_6c_9a_12.csv")

if (file.exists(r3_path)) {
  r3_raw <- read_csv(r3_path, show_col_types = FALSE)

  # All R3 households are panel respondents; treat all as "had enterprise potential"
  # Test: do R3 households absent from R5 enterprise sample differ by fintech?
  r3_ent <- r3_raw |>
    mutate(state_f = factor(state), had_enterprise_r3 = 1L) |>
    select(hhid, state, state_f, had_enterprise_r3)

  active_r5_hh <- fp |> filter(round == "R5") |>
    distinct(hhid) |> mutate(in_r5 = 1L)

  ft_raw <- read_csv(file.path(root, "data/instruments/preschock_fintech_state.csv"),
                     show_col_types = FALSE) |>
    mutate(fintech_std = as.numeric(scale(pct_fintech_index))) |>
    select(state_code, fintech_std)

  attrition_df <- r3_ent |>
    left_join(active_r5_hh, by = "hhid") |>
    mutate(
      in_r5        = replace_na(in_r5, 0L),
      dropped_by_r5 = 1L - in_r5
    ) |>
    left_join(ft_raw |> rename(state = state_code), by = "state") |>
    filter(!is.na(fintech_std))

  m_attrition <- feols(
    dropped_by_r5 ~ fintech_std,
    data = attrition_df, cluster = ~state_f
  )

  cat("\n=== Attrition test (R3 households → R5 sample) ===\n")
  cat("Fintech coef on dropout:", round(coef(m_attrition)["fintech_std"],4),
      "p:", round(pvalue(m_attrition)["fintech_std"],3), "\n")
  cat("Attrition rate:", round(mean(attrition_df$dropped_by_r5, na.rm=TRUE),3), "\n")

  atr_tex <- c(
    "\\begin{table}[H]",
    "\\begin{threeparttable}",
    "\\caption{Attrition Test: Pre-Baseline Household Dropout and Fintech}",
    "\\label{atab:attrition}",
    "\\small\\begin{tabular}{lcc}",
    "\\toprule",
    " & Coef. on Fintech & $p$-value \\\\",
    "\\midrule",
    sprintf("Dropped from R3 to R5 baseline & %.4f & %.3f%s \\\\",
            coef(m_attrition)["fintech_std"],
            pvalue(m_attrition)["fintech_std"],
            star_fn(pvalue(m_attrition)["fintech_std"])),
    "\\bottomrule",
    "\\end{tabular}",
    "\\begin{tablenotes}\\footnotesize",
    paste0(
      "\\item \\textit{Notes:} Dependent variable = 1 if a household present in",
      " NLPS Round 3 (approx.\\ Aug 2021) is absent from the Round 5",
      " (Aug 2022) active enterprise sample.",
      " Regressor is the standardised state fintech index.",
      " A coefficient close to zero confirms that pre-baseline attrition",
      " is uncorrelated with the treatment variable."
    ),
    "\\end{tablenotes}\\end{threeparttable}\\end{table}"
  )
  writeLines(atr_tex, file.path(tab_dir, "atab_attrition.tex"))
  cat("Saved: atab_attrition.tex\n")
} else {
  cat("R3 file not found — attrition table skipped.\n")
}

# ── 6. Sales null decomposition ────────────────────────────────────────────────
# Compare R5 baseline ln_sales for R7-survivors vs R7-exiters by fintech tercile.
# Survivor selection: if low-fintech states have positively selected survivors,
# R5 sales of survivors should exceed exiters MORE in low-fintech states.

fp_r5_r7 <- fp |>
  filter(round %in% c("R5","R7")) |>
  select(hhid, round, active, ln_sales, fintech_std, state_f, fintech_tercile) |>
  pivot_wider(names_from = round, values_from = c(active, ln_sales),
              names_sep = "_")

fp_r5_r7 <- fp_r5_r7 |>
  mutate(
    survived_r7 = as.integer(!is.na(active_R7) & active_R7 == 1),
    ft_group    = case_when(
      fintech_tercile == 1 ~ "Low fintech",
      fintech_tercile == 2 ~ "Mid fintech",
      fintech_tercile == 3 ~ "High fintech"
    )
  ) |>
  filter(!is.na(ln_sales_R5) & !is.na(survived_r7))

# Mean R5 ln_sales by survival status and fintech tercile
surv_table <- fp_r5_r7 |>
  group_by(ft_group, survived_r7) |>
  summarise(
    mean_ln_sales_r5 = mean(ln_sales_R5, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(survival = if_else(survived_r7 == 1, "Survived to R7", "Exited by R7"))

cat("\n=== Sales null: survivor selection by fintech tercile ===\n")
print(surv_table |> select(ft_group, survival, mean_ln_sales_r5, n) |>
      arrange(ft_group, survival))

# Regression: R5 sales ~ survived × fintech interaction (linear cross-section)
m_sel <- feols(
  ln_sales_R5 ~ survived_r7 * fintech_std,
  data = fp_r5_r7, cluster = ~state_f
)
cat("\nSurvivor selection interaction (R5 sales ~ survived × fintech):\n")
cat("  survived_r7 coef:", round(coef(m_sel)["survived_r7"], 4),
    "p:", round(pvalue(m_sel)["survived_r7"], 3), "\n")
cat("  survived×fintech:", round(coef(m_sel)["survived_r7:fintech_std"], 4),
    "p:", round(pvalue(m_sel)["survived_r7:fintech_std"], 3), "\n")

# Export survival table
sel_tex <- c(
  "\\begin{table}[H]",
  "\\begin{threeparttable}",
  "\\caption{Survivor Selection and the Sales Null: Pre-Shock Sales by Fintech Tercile}",
  "\\label{atab:sales_decomp}",
  "\\small\\begin{tabular}{lcccc}",
  "\\toprule",
  "Fintech group & Survived to R7 & Exited by R7 & Difference & $N$ (surv/exit) \\\\",
  "\\midrule"
)
for (grp in c("Low fintech","Mid fintech","High fintech")) {
  s_row <- surv_table |> filter(ft_group == grp & survived_r7 == 1)
  e_row <- surv_table |> filter(ft_group == grp & survived_r7 == 0)
  if (nrow(s_row) == 0 || nrow(e_row) == 0) next
  diff_val <- s_row$mean_ln_sales_r5 - e_row$mean_ln_sales_r5
  sel_tex <- c(sel_tex, sprintf(
    "%s & %.3f & %.3f & %.3f & %d / %d \\\\",
    grp, s_row$mean_ln_sales_r5, e_row$mean_ln_sales_r5,
    diff_val, s_row$n, e_row$n
  ))
}
coef_int <- coef(m_sel)["survived_r7:fintech_std"]
p_int    <- pvalue(m_sel)["survived_r7:fintech_std"]
sel_tex <- c(sel_tex,
  "\\midrule",
  sprintf("Interaction coef.\\ (survived $\\times$ fintech): & \\multicolumn{3}{c}{%.4f%s} & \\\\",
          coef_int, star_fn(p_int)),
  sprintf("$p$-value: & \\multicolumn{3}{c}{%.3f} & \\\\", p_int),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Mean log weekly sales at Round 5 (Aug 2022 pre-shock baseline)",
    " for enterprises that survived to R7 vs.\\ those that had exited, by fintech tercile.",
    " If the log-sales null result at R7 reflects positive survivor selection in",
    " low-fintech states, the survival premium (survived minus exited)",
    " should be \\emph{larger} in the Low fintech group.",
    " The interaction coefficient from OLS of R5 sales on survival status",
    " interacted with the continuous fintech index tests this formally;",
    " a negative coefficient would confirm stronger positive selection in low-fintech states.",
    " Standard errors clustered at the state level."
  ),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
writeLines(sel_tex, file.path(tab_dir, "atab_sales_decomp.tex"))
cat("Saved: atab_sales_decomp.tex\n")

# ── 7. Welfare weights: implied aggregate enterprise losses ────────────────────
# Use sampling weights from R5 to estimate total enterprise count
r5_raw <- read_csv(
  file.path(nlps, "p2r5_sect_a_2_5_6_9a_11b_13_12.csv"),
  show_col_types = FALSE
)

total_enterprise_wt <- r5_raw |>
  filter(!is.na(s13q1) & s13q1 == 1) |>   # active enterprise
  summarise(total = sum(wt_p2round5, na.rm = TRUE)) |>
  pull(total)

cat(sprintf("\n=== Welfare calculation ===\n"))
cat(sprintf("Estimated total non-farm enterprise HHs (weighted, R5): %.0f\n",
            total_enterprise_wt))

act_r7_coef  <- coef(m_act1)["round::R7:fintech_std"]
act_r11_coef <- coef(m_act1)["round::R11:fintech_std"]
wk_r7_coef   <- coef(m_wk1)["round::R7:fintech_std"]

# 10th→90th percentile fintech gap
ft_p10 <- quantile(fp$fintech_std[fp$round=="R5"], 0.10, na.rm=TRUE)
ft_p90 <- quantile(fp$fintech_std[fp$round=="R5"], 0.90, na.rm=TRUE)
ft_gap  <- ft_p90 - ft_p10

enterprises_saved_r7  <- act_r7_coef  * ft_gap * total_enterprise_wt
enterprises_saved_r11 <- act_r11_coef * ft_gap * total_enterprise_wt
workers_saved_r7      <- wk_r7_coef   * ft_gap * total_enterprise_wt

cat(sprintf("10th→90th fintech gap: %.3f SD\n", ft_gap))
cat(sprintf("Enterprise exits averted at peak (R7):   %.0f\n", enterprises_saved_r7))
cat(sprintf("Enterprise exits averted at medium-run:  %.0f\n", enterprises_saved_r11))
cat(sprintf("Worker-jobs preserved at peak (R7):      %.0f\n", workers_saved_r7))

saveRDS(
  list(
    act_r7_coef  = act_r7_coef,
    act_r7_p     = pvalue(m_act1)["round::R7:fintech_std"],
    act_r11_coef = act_r11_coef,
    act_r11_p    = pvalue(m_act1)["round::R11:fintech_std"],
    sal_r7_coef  = coef(m_sal1)["round::R7:fintech_std"],
    sal_r7_p     = pvalue(m_sal1)["round::R7:fintech_std"],
    wk_r7_coef   = wk_r7_coef,
    wk_r7_p      = pvalue(m_wk1)["round::R7:fintech_std"],
    n_firms      = n_distinct(fp$hhid),
    total_enterprise_wt = total_enterprise_wt,
    ft_gap              = ft_gap,
    enterprises_saved_r7  = enterprises_saved_r7,
    enterprises_saved_r11 = enterprises_saved_r11,
    workers_saved_r7      = workers_saved_r7,
    # Survivor selection
    sel_interaction_coef = coef(m_sel)["survived_r7:fintech_std"],
    sel_interaction_p    = pvalue(m_sel)["survived_r7:fintech_std"]
  ),
  file.path(root, "data/processed/key_numbers_firm.rds")
)
cat("Script 11 complete.\n")
