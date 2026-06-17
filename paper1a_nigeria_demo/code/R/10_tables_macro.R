# Script 10: Export all macro tables via fixest::etable()
# Produces: tab_did_macro.tex, tab_summary_stats.tex

library(readr); library(dplyr); library(fixest); library(tidyr)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
tab_dir <- file.path(root, "paper/tables")
dir.create(tab_dir, showWarnings = FALSE)

sp <- read_csv(
  file.path(root, "data/processed/analysis_panel_state.csv"),
  show_col_types = FALSE
) |>
  mutate(
    date_fct    = factor(format(as.Date(date), "%Y-%m")),
    fintech_std = as.numeric(scale(pct_fintech_index)),
    period = factor(case_when(
      t_rel %in% c(-4, -3) ~ "announcement",
      t_rel %in% c(-2, -1) ~ "transition",
      t_rel %in%  0:1      ~ "peak",
      t_rel %in%  2:6      ~ "recovery",
      t_rel >=  7          ~ "post",
      TRUE                  ~ "pre"
    ), levels = c("pre","announcement","transition","peak","recovery","post"))
  )

# ── Pre-shock NTL control ─────────────────────────────────────────────────────
ntl_base <- sp |>
  filter(t_rel < -4) |>
  group_by(state) |>
  summarise(ntl_base = mean(ln_ntl_mean, na.rm = TRUE), .groups = "drop")

sp <- sp |>
  left_join(ntl_base, by = "state") |>
  mutate(ntl_base_x_post = ntl_base * as.integer(t_rel >= -4))

wbes_states <- c("Abia","Abuja","Anambra","Cross river","Enugu","Gombe",
                 "Jigawa","Kaduna","Kano","Katsina","Kebbi","Kwara",
                 "Lagos","Nasarawa","Niger","Ogun","Oyo","Sokoto","Zamfara")

# ── Models ────────────────────────────────────────────────────────────────────
mod1 <- feols(
  ln_ntl_mean ~ i(period, fintech_std, ref = "pre") | state + date_fct,
  data = sp, cluster = ~state
)
mod2 <- feols(
  ln_ntl_mean ~ i(period, fintech_std, ref = "pre") | state + date_fct,
  data = sp |> filter(state %in% wbes_states), cluster = ~state
)
mod3 <- feols(
  ln_ntl_mean ~ i(period, fintech_std, ref = "pre") +
    ntl_base_x_post | state + date_fct,
  data = sp, cluster = ~state
)

cat("=== DiD Macro Results ===\n")
for (m in list(mod1, mod2, mod3)) {
  pk <- grep("peak", names(coef(m)), value = TRUE)
  if (length(pk)) cat(sprintf("  Peak: %.4f (SE %.4f, p=%.3f)\n",
    coef(m)[pk], se(m)[pk], pvalue(m)[pk]))
}

# ── Export DiD table ──────────────────────────────────────────────────────────
dict_main <- c(
  "period::announcement:fintech_std" = "Fintech $\\times$ Announcement",
  "period::transition:fintech_std"   = "Fintech $\\times$ Transition",
  "period::peak:fintech_std"         = "Fintech $\\times$ Peak crunch",
  "period::recovery:fintech_std"     = "Fintech $\\times$ Recovery",
  "period::post:fintech_std"         = "Fintech $\\times$ Post-shock",
  "ntl_base_x_post"                  = "Baseline NTL $\\times$ Post"
)

etable(
  mod1, mod2, mod3,
  title     = "Differential Effect of Pre-Shock Fintech Adoption on Nighttime Lights",
  headers   = list("^" = .("Baseline", "WBES states", "Controls")),
  keep      = "%period::",
  dict      = dict_main,
  signif.code = c("***" = .01, "**" = .05, "*" = .10),
  extralines = list(
    "State FE"          = rep("\\checkmark", 3),
    "Month-Year FE"     = rep("\\checkmark", 3),
    "Pre-NTL control"   = c("No","No","Yes"),
    "States"            = c("37","19","37")
  ),
  notes = paste(
    "Dependent variable: $\\ln(\\text{NTL}_{st}+1)$.",
    "Treatment is the standardised pre-shock digital payment index",
    "from NLPS Round 6 (October 2022). Pre-shock period omitted.",
    "Standard errors clustered at the state level."
  ),
  file   = file.path(tab_dir, "tab_did_macro.tex"),
  tex    = TRUE,
  replace = TRUE
)
cat("Saved: tab_did_macro.tex\n")

# ── Summary statistics table (manual, threeparttable-compatible) ──────────────
sum_vars <- sp |>
  select(ln_ntl_mean, fintech_std, pct_fintech_index,
         pct_unbanked, mean_cash_dep) |>
  summarise(across(everything(), list(
    Mean = ~mean(.x, na.rm=TRUE),
    SD   = ~sd(.x, na.rm=TRUE),
    Min  = ~min(.x, na.rm=TRUE),
    Max  = ~max(.x, na.rm=TRUE)
  ))) |>
  pivot_longer(everything(),
    names_to  = c("Variable", ".value"),
    names_sep = "_(?=[^_]+$)"
  )

cat("\nSummary stats:\n")
print(sum_vars |> mutate(across(where(is.numeric), \(x) round(x, 3))))

# Save key numbers
saveRDS(
  list(
    peak_b1  = coef(mod1)[grep("peak", names(coef(mod1)))],
    peak_se1 = se(mod1)[grep("peak", names(se(mod1)))],
    peak_p1  = pvalue(mod1)[grep("peak", names(pvalue(mod1)))],
    post_b1  = coef(mod1)[grep("post", names(coef(mod1)))],
    n_states = 37L,
    n_obs    = nobs(mod1)
  ),
  file.path(root, "data/processed/key_numbers_macro.rds")
)
cat("Script 10 complete.\n")
