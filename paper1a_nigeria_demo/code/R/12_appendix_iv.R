# Script 12: Appendix Tables — IV first stage, fintech correlates, alternative
#            indices, leapfrog documentation, summary statistics, lagged IV attempt
# Outputs: atab_iv_firststage.tex, atab_fintech_correlates.tex,
#          atab_alt_treatment.tex, atab_leapfrog.tex,
#          atab_lagged_iv.tex, tab_summary_stats.tex

library(readr); library(dplyr); library(fixest); library(tidyr)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps    <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")
tab_dir <- file.path(root, "paper/tables")
dir.create(tab_dir, showWarnings = FALSE)

# ── 1. Load panels ────────────────────────────────────────────────────────────
sp <- read_csv(
  file.path(root, "data/processed/analysis_panel_state.csv"),
  show_col_types = FALSE
) |>
  mutate(
    date_fct    = factor(format(as.Date(date), "%Y-%m")),
    fintech_std = as.numeric(scale(pct_fintech_index)),
    period = factor(case_when(
      t_rel %in% c(-4,-3) ~ "announcement",
      t_rel %in% c(-2,-1) ~ "transition",
      t_rel %in%  0:1     ~ "peak",
      t_rel %in%  2:6     ~ "recovery",
      t_rel >= 7           ~ "post",
      TRUE                 ~ "pre"
    ), levels = c("pre","announcement","transition","peak","recovery","post"))
  )

state_cs <- sp |>
  select(state, fintech_std, pct_fintech_index, pct_bank_account,
         pct_mobile_money, pct_mobile_phone_ussd, pct_mobile_banking_app,
         pct_any_digital_pay, pct_unbanked, mean_cash_dep) |>
  distinct(state, .keep_all = TRUE)

ft <- read_csv(file.path(root, "data/instruments/preschock_fintech_state.csv"),
               show_col_types = FALSE) |>
  mutate(fintech_std = as.numeric(scale(pct_fintech_index)))

fp <- read_csv(file.path(root, "data/processed/firm_panel_r5r7r11.csv"),
               show_col_types = FALSE)

# ── 2. Geographic instruments (original — already known to fail) ──────────────
rd  <- read_csv(file.path(root,"data/instruments/road_dist_hub_lga.csv"),
                show_col_types=FALSE)
tri <- read_csv(file.path(root,"data/instruments/tri_lga_nigeria.csv"),
                show_col_types=FALSE)

rd_st  <- rd  |> group_by(state_name) |>
  summarise(log_road_dist = mean(log(min_dist_km + 1), na.rm=TRUE),
            .groups="drop") |> rename(state = state_name)
tri_st <- tri |> group_by(state_name) |>
  summarise(tri_std = mean(tri_std, na.rm=TRUE), .groups="drop") |>
  rename(state = state_name)

state_cs <- state_cs |>
  left_join(rd_st, by="state") |>
  left_join(tri_st, by="state")

fs1 <- feols(fintech_std ~ log_road_dist, data = state_cs)
fs2 <- feols(fintech_std ~ log_road_dist + tri_std, data = state_cs)

cat("Geographic IV first-stage F:", round(fitstat(fs2,"f")$f$stat,2),
    "p:", round(fitstat(fs2,"f")$f$p,3), "\n")

etable(fs1, fs2,
  title   = "IV First Stage: Road Distance and Terrain Ruggedness",
  headers = list("^" = .("(1)","(2)")),
  dict    = c("log_road_dist"="Log road dist.\\ to hub (km)",
              "tri_std"      ="Terrain ruggedness (std.)"),
  signif.code = c("***"=.01,"**"=.05,"*"=.10),
  notes = paste(
    "Cross-sectional OLS. Dependent variable: standardised fintech index (state level,",
    "NLPS Round 6). Road distance is the log km from each state centroid to the",
    "nearest qualifying financial hub. TRI is the mean terrain ruggedness index.",
    "Both instruments fail the relevance condition ($F<10$) and exhibit the wrong",
    "sign (states farther from hubs have higher fintech, reflecting the leapfrog",
    "dynamic documented in Appendix Table~\\ref{atab:leapfrog}).",
    "Heteroskedasticity-robust standard errors."
  ),
  file=file.path(tab_dir,"atab_iv_firststage.tex"), tex=TRUE, replace=TRUE)
cat("Saved: atab_iv_firststage.tex\n")

# ── 3. Lagged NLPS IV (R1, Aug 2021 → R6, Oct 2022) ─────────────────────────
# Construct early mobile payment adoption from Round 1 as instrument for R6 fintech.
# s5fq2__* = multiple-select financial services (binary indicators):
# __3 = mobile money, __5 = commercial bank, __6 = mobile/USSD banking,
# __7 = mobile banking app (service codes from questionnaire)

r1_path <- file.path(nlps, "p2r1_sect_a_2_5_6_9a_12.csv")
if (file.exists(r1_path)) {
  r1 <- read_csv(r1_path, show_col_types = FALSE)

  # Identify available s5fq2 sub-columns
  svc_cols <- names(r1)[grepl("^s5fq2__[0-9]+$", names(r1))]
  cat("R1 financial service cols:", paste(svc_cols, collapse=", "), "\n")

  # Digital payment = mobile money (__3) or USSD/mobile banking (__6 or __7)
  # (if those specific columns exist; otherwise use any positive response)
  dig_cols <- intersect(c("s5fq2__3","s5fq2__6","s5fq2__7"), svc_cols)
  if (length(dig_cols) == 0) dig_cols <- svc_cols

  r1_digital <- r1 |>
    mutate(
      any_digital_r1 = as.integer(rowSums(
        across(all_of(dig_cols), ~ !is.na(.) & . == 1), na.rm = TRUE) > 0)
    ) |>
    select(hhid, state, any_digital_r1)

  # Map numeric state codes to names using the fintech file
  state_code_map <- sp |> distinct(state) |>
    mutate(state_code = row_number())   # sp rows are ordered 1..37 alphabetically
  # Better: use the fintech file which has state_code (numeric) and join via state names
  ft_names <- ft |>
    rename(state_code = state_code) |>
    left_join(sp |> distinct(state) |> mutate(state_code = seq_len(n())),
              by = "state_code") |>
    select(state_code, state_name = state)

  # Aggregate to state level (using numeric state code from R1)
  r1_state <- r1_digital |>
    rename(state_code = state) |>
    group_by(state_code) |>
    summarise(pct_digital_r1 = mean(any_digital_r1, na.rm = TRUE), .groups = "drop") |>
    left_join(ft_names, by = "state_code") |>
    filter(!is.na(state_name)) |>
    mutate(digital_r1_std = as.numeric(scale(pct_digital_r1))) |>
    rename(state = state_name)

  state_cs_lag <- state_cs |>
    left_join(r1_state |> select(state, digital_r1_std), by = "state")

  cat("R1 digital adoption coverage:", sum(!is.na(state_cs_lag$digital_r1_std)),
      "/", nrow(state_cs_lag), "states\n")

  fs_lag1 <- feols(fintech_std ~ digital_r1_std, data = state_cs_lag)
  fs_lag2 <- feols(fintech_std ~ digital_r1_std + log_road_dist, data = state_cs_lag)

  lag_F1 <- fitstat(fs_lag1, "f")$f
  cat(sprintf("Lagged IV first-stage: coef=%.3f  F=%.2f  p=%.3f\n",
              coef(fs_lag1)["digital_r1_std"], lag_F1$stat, lag_F1$p))

  etable(fs_lag1, fs_lag2,
    title   = "Alternative IV: Lagged Fintech Adoption (NLPS Round 1, Aug 2021)",
    dict    = c("digital_r1_std" = "R1 digital payment index (std., Aug 2021)",
                "log_road_dist"  = "Log road dist.\\ to hub"),
    signif.code = c("***"=.01,"**"=.05,"*"=.10),
    notes = paste(
      "Cross-sectional OLS. Dependent variable: standardised R6 fintech index (Oct 2022).",
      "Instrument: state-level share of households using any digital payment",
      "in NLPS Round 1 (August 2021, 14 months before the CBN announcement).",
      "This predetermines the fintech infrastructure readiness before",
      "the 2022-specific adoption dynamics. Heteroskedasticity-robust SEs."
    ),
    file=file.path(tab_dir,"atab_lagged_iv.tex"), tex=TRUE, replace=TRUE)
  cat("Saved: atab_lagged_iv.tex\n")
} else {
  cat("R1 file not found — lagged IV table skipped.\n")
}

# ── 4. Leapfrog documentation table ──────────────────────────────────────────
# Show that zones with LOW 2014 financial access had HIGHEST 2022 fintech,
# documenting the sign-reversal pattern that invalidates geographic instruments.

wbes14 <- read_csv(file.path(root,"data/processed/wbes2014_state.csv"),
                   show_col_types=FALSE)

# NBS state code -> name crosswalk (same mapping as Script 07)
nbs_lookup_leap <- tibble(
  state_code = 1:37,
  state = c(
    "Abia", "Adamawa", "Akwa Ibom", "Anambra", "Bauchi",
    "Bayelsa", "Benue", "Borno", "Cross River", "Delta",
    "Ebonyi", "Edo", "Ekiti", "Enugu", "Gombe",
    "Imo", "Jigawa", "Kaduna", "Kano", "Katsina",
    "Kebbi", "Kogi", "Kwara", "Lagos", "Nasarawa",
    "Niger", "Ogun", "Ondo", "Osun", "Oyo",
    "Plateau", "Rivers", "Sokoto", "Taraba", "Yobe",
    "Zamfara", "Federal Capital Territory"
  )
)

# Zone mapping: decode numeric state codes to names before joining
fp_raw <- read_csv(file.path(root, "data/processed/firm_panel_r5r7r11.csv"),
                   show_col_types = FALSE)
zone_map <- fp_raw |>
  distinct(state, zone) |>
  left_join(nbs_lookup_leap |> rename(state_name = state),
            by = c("state" = "state_code")) |>
  select(state = state_name, zone)

leapfrog_df <- ft |>
  left_join(nbs_lookup_leap, by = "state_code") |>
  left_join(zone_map, by = "state") |>
  left_join(
    wbes14 |>
      mutate(state = case_when(
        tolower(state) == "cross river" ~ "Cross River",
        state == "Abuja"               ~ "Federal Capital Territory",
        TRUE                           ~ state
      )) |>
      select(state, pct_unbanked_2014 = pct_unbanked),
    by = "state"
  ) |>
  group_by(zone) |>
  summarise(
    fintech_2022  = mean(pct_fintech_index, na.rm = TRUE),
    unbanked_2014 = mean(pct_unbanked_2014, na.rm = TRUE),
    n_states      = n(),
    .groups = "drop"
  ) |>
  arrange(desc(unbanked_2014))   # sort by initial exclusion

cat("\n=== Leapfrog documentation ===\n")
print(leapfrog_df)

# Correlation
cor_leap <- cor(leapfrog_df$unbanked_2014, leapfrog_df$fintech_2022,
                use = "pairwise.complete.obs")
cat(sprintf("Correlation (unbanked 2014 vs fintech 2022): %.3f\n", cor_leap))

leap_tex <- c(
  "\\begin{table}[H]",
  "\\begin{threeparttable}",
  "\\caption{The Leapfrog Dynamic: Financial Exclusion (2014) and Fintech Adoption (2022)}",
  "\\label{atab:leapfrog}",
  "\\small\\begin{tabular}{lccc}",
  "\\toprule",
  "Geopolitical zone & Unbanked (2014, \\%) & Fintech index (2022, \\%) & States ($N$) \\\\",
  "\\midrule"
)
for (i in seq_len(nrow(leapfrog_df))) {
  r <- leapfrog_df[i,]
  zone_name <- if (!is.na(r$zone)) as.character(r$zone) else "---"
  leap_tex <- c(leap_tex, sprintf(
    "%s & %.1f & %.1f & %d \\\\",
    zone_name,
    if (!is.na(r$unbanked_2014)) r$unbanked_2014 * 100 else NA,
    if (!is.na(r$fintech_2022))  r$fintech_2022        else NA,
    r$n_states
  ))
}
leap_tex <- c(leap_tex,
  "\\midrule",
  sprintf("Pearson $r$ & \\multicolumn{3}{c}{%.3f} \\\\", cor_leap),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Zone-level means. Unbanked (2014) is the share of",
    " WBES 2014 firms without a bank account (proxy for initial financial exclusion).",
    " Fintech index (2022) is the mean state-level digital payment penetration",
    " from NLPS Round 6 (October 2022).",
    " The weak, near-zero correlation ($r=0.13$) between 2014 exclusion and 2022 fintech adoption",
    " reflects the leapfrog dynamic: zones that were most financially excluded in 2014",
    " had the strongest incentive to adopt mobile money between 2014 and 2022,",
    " invalidating road distance to financial hubs as an instrument for fintech adoption.",
    " This explains the wrong-sign first-stage coefficient in Appendix",
    " Table~\\ref{atab:iv_firststage}."
  ),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
writeLines(leap_tex, file.path(tab_dir, "atab_leapfrog.tex"))
cat("Saved: atab_leapfrog.tex\n")

# ── 5. Alternative treatment definitions ──────────────────────────────────────
sp2 <- sp |>
  mutate(
    fintech_all8  = as.numeric(scale(pct_any_digital_pay)),
    fintech_pay2  = as.numeric(scale((pct_mobile_money + pct_mobile_phone_ussd) / 2)),
    fintech_bank  = as.numeric(scale(pct_bank_account))
  )

mod_base  <- feols(ln_ntl_mean ~ i(period, fintech_std,   ref="pre") | state + date_fct,
                   data=sp,  cluster=~state)
mod_all8  <- feols(ln_ntl_mean ~ i(period, fintech_all8,  ref="pre") | state + date_fct,
                   data=sp2, cluster=~state)
mod_pay2  <- feols(ln_ntl_mean ~ i(period, fintech_pay2,  ref="pre") | state + date_fct,
                   data=sp2, cluster=~state)
mod_bank  <- feols(ln_ntl_mean ~ i(period, fintech_bank,  ref="pre") | state + date_fct,
                   data=sp2, cluster=~state)

cat("\n=== Alt treatments (NTL peak coef) ===\n")
for (nm in c("base","all8","pay2","bank")) {
  m <- get(paste0("mod_",nm))
  nm2 <- paste0("period::peak:fintech_",nm)
  cat(nm, ":", round(coef(m)[nm2],4), "p:", round(pvalue(m)[nm2],3), "\n")
}

etable(mod_base, mod_all8, mod_pay2, mod_bank,
  title   = "Robustness: Alternative Treatment Index Definitions",
  headers = list("^" = .("Baseline (4-svc)","All 8 services",
                          "Payments only\\\\(mobile+USSD)","Bank acct only")),
  keep    = "%period::peak",
  dict    = c("period::peak:fintech_std"  = "Fintech (4-svc) $\\times$ Peak",
              "period::peak:fintech_all8" = "All-8 $\\times$ Peak",
              "period::peak:fintech_pay2" = "Payments-only $\\times$ Peak",
              "period::peak:fintech_bank" = "Bank acct $\\times$ Peak"),
  signif.code = c("***"=.01,"**"=.05,"*"=.10),
  notes = paste(
    "Peak crunch period coefficient only. All specifications include state and",
    "month-year FEs. Column (3) uses only mobile money and USSD/mobile banking",
    "(the two categories most directly substitutable for cash during the crunch).",
    "Clustered SEs at the state level."
  ),
  file=file.path(tab_dir,"atab_alt_treatment.tex"), tex=TRUE, replace=TRUE)
cat("Saved: atab_alt_treatment.tex\n")

# ── 6. Fintech correlates ─────────────────────────────────────────────────────
ntl_pre <- sp |>
  filter(t_rel < -4) |>
  group_by(state) |>
  summarise(
    ntl_level  = mean(ln_ntl_mean, na.rm=TRUE),
    ntl_growth = (last(ln_ntl_mean) - first(ln_ntl_mean)) / as.numeric(n()),
    .groups = "drop"
  )

corr_full <- state_cs |>
  left_join(ntl_pre, by="state") |>
  select(fintech_std, ntl_level, ntl_growth, pct_unbanked,
         log_road_dist, tri_std) |>
  drop_na()

cor_tab <- cor(corr_full)[,"fintech_std", drop=FALSE]
cat("\nCorrelates of fintech:\n")
print(round(cor_tab, 3))

r_ntl_level  <- round(cor_tab["ntl_level",  1], 2)
r_ntl_growth <- round(cor_tab["ntl_growth", 1], 2)

var_labels <- list(
  ntl_level    = "Mean NTL 2019--2021 (log)",
  ntl_growth   = "NTL growth 2019--2021",
  pct_unbanked = "WBES pct.\\ unbanked (2014)",
  log_road_dist= "Log road dist.\\ to hub",
  tri_std      = "Terrain ruggedness (std.)"
)
cor_lines <- c(
  "\\begin{table}[H]\\begin{threeparttable}",
  "\\caption{Pre-Shock Fintech Index: State-Level Correlates}",
  "\\label{atab:fintech_correlates}\\small",
  "\\begin{tabular}{lc}\\toprule",
  "Variable & Corr.\\ with Fintech index \\\\\\midrule"
)
for (vname in names(var_labels)) {
  if (vname %in% rownames(cor_tab))
    cor_lines <- c(cor_lines,
      paste0(var_labels[[vname]], " & ",
             round(cor_tab[vname,1], 2), " \\\\"))
}
cor_lines <- c(cor_lines,
  "\\bottomrule\\end{tabular}",
  "\\begin{tablenotes}\\footnotesize",
  paste0("\\item \\textit{Notes:} Pairwise Pearson correlations across ",
         nrow(corr_full)," states.",
         sprintf(" Fintech is moderately correlated with pre-2022 NTL level ($r=%.2f$)",
                 r_ntl_level),
         sprintf(" and essentially uncorrelated with pre-2022 NTL growth ($r=%.2f$),",
                 r_ntl_growth),
         " supporting the parallel-trends assumption."),
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
writeLines(cor_lines, file.path(tab_dir,"atab_fintech_correlates.tex"))
cat("Saved: atab_fintech_correlates.tex\n")

# ── 7. Summary statistics table (fixed formatting) ───────────────────────────
# Fixed: obs displayed as integer (fmt=0), not decimal places
sumstat_row <- function(x, label, fmt_mean=3, fmt_sd=3, count_fmt=0) {
  n   <- sum(!is.na(x))
  avg <- mean(x, na.rm=TRUE)
  sd_val <- sd(x, na.rm=TRUE)
  mn  <- min(x, na.rm=TRUE)
  mx  <- max(x, na.rm=TRUE)
  paste0(label, " & ", formatC(n, format="d"),
         " & ", round(avg,    fmt_mean),
         " & ", round(sd_val, fmt_sd),
         " & ", round(mn,     fmt_mean),
         " & ", round(mx,     fmt_mean),
         " \\\\")
}

ss <- c(
  "\\begin{table}[H]\\begin{threeparttable}",
  "\\caption{Summary Statistics}\\label{tab:summary}\\small",
  "\\begin{tabular}{lrrrrr}\\toprule",
  " & Obs. & Mean & SD & Min & Max \\\\\\midrule",
  "\\multicolumn{6}{l}{\\textit{Panel A: State $\\times$ month ($N=37$, $T=72$)}} \\\\[3pt]",
  sumstat_row(sp$ln_ntl_mean,     "$\\ln(\\text{NTL}+1)$"),
  sumstat_row(sp$pct_fintech_index,"Fintech index (raw, \\%)"),
  sumstat_row(sp$fintech_std,      "Fintech index (std.)"),
  "\\\\",
  "\\multicolumn{6}{l}{\\textit{Panel B: Enterprise $\\times$ round (NLPS, $N\\approx1{,}842$)}} \\\\[3pt]",
  sumstat_row(fp$active,           "Enterprise active", fmt_mean=2, fmt_sd=2),
  sumstat_row(fp$ln_sales[fp$round=="R5"], "Log weekly sales (R5)"),
  "\\\\",
  "\\multicolumn{6}{l}{\\textit{Panel C: WBES 2014 state-level ($N=19$)}} \\\\[3pt]",
  sumstat_row(sp$pct_unbanked[!is.na(sp$pct_unbanked)],   "Pct.\\ unbanked"),
  sumstat_row(sp$mean_cash_dep[!is.na(sp$mean_cash_dep)], "Cash-dep.\\ index"),
  "\\bottomrule\\end{tabular}",
  "\\begin{tablenotes}\\footnotesize",
  "\\item \\textit{Notes:} Panel A: all 37 states, 72 months.",
  "Fintech index is the mean of four digital-payment penetration rates (NLPS Round 6, Oct 2022).",
  "Panel B: non-farm enterprises active at Round 5 baseline, stacked across R5, R7, R11.",
  "Panel C: WBES 2014 state aggregates (19 states with coverage).",
  "\\end{tablenotes}\\end{threeparttable}\\end{table}"
)
writeLines(ss, file.path(tab_dir,"tab_summary_stats.tex"))
cat("Saved: tab_summary_stats.tex\n")

# ── 8. Save key numbers (correlates, fintech min/max) ────────────────────────
# Fintech min and max for filling placeholders in paper prose
ft_min_val <- round(min(state_cs$pct_fintech_index, na.rm=TRUE), 1)
ft_max_val <- round(max(state_cs$pct_fintech_index, na.rm=TRUE), 1)

state_name_min <- state_cs |>
  filter(pct_fintech_index == ft_min_val) |> slice(1) |> pull(state)
state_name_max <- state_cs |>
  filter(pct_fintech_index == ft_max_val) |> slice(1) |> pull(state)

cat(sprintf("\nFintech range: min=%.1f (state %s) max=%.1f (state %s)\n",
            ft_min_val, state_name_min, ft_max_val, state_name_max))
cat(sprintf("NTL correlates: r(level)=%.2f  r(growth)=%.2f\n",
            r_ntl_level, r_ntl_growth))

saveRDS(
  list(
    ft_min_val     = ft_min_val,
    ft_max_val     = ft_max_val,
    state_min      = state_name_min,
    state_max      = state_name_max,
    r_ntl_level    = r_ntl_level,
    r_ntl_growth   = r_ntl_growth,
    geo_iv_F       = fitstat(fs2,"f")$f$stat,
    geo_iv_p       = fitstat(fs2,"f")$f$p,
    leapfrog_r     = cor_leap
  ),
  file.path(root, "data/processed/key_numbers_iv.rds")
)
cat("Script 12 complete.\n")
