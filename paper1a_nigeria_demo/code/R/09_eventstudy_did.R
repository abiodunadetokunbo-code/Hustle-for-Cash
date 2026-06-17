# Script 09: VIIRS Event Study + DiD Macro Table
# Outputs: paper/figures/fig_eventstudy.pdf
#          paper/figures/fig_eventstudy_trend.pdf   (state-trend robustness)
#          paper/tables/tab_did_macro.tex

library(readr); library(dplyr); library(fixest)
library(ggplot2); library(modelsummary); library(patchwork)
options(modelsummary_factory_latex = "kableExtra")

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
fig_dir <- file.path(root, "paper/figures")
tab_dir <- file.path(root, "paper/tables")
dir.create(fig_dir, showWarnings = FALSE)
dir.create(tab_dir, showWarnings = FALSE)

# ── 1. Load state panel ───────────────────────────────────────────────────────
sp <- read_csv(
  file.path(root, "data/processed/analysis_panel_state.csv"),
  show_col_types = FALSE
) |>
  mutate(
    date_fct    = factor(format(as.Date(date), "%Y-%m")),
    # Numeric time index (for state-specific linear trend)
    t_num       = as.integer(factor(date_fct, levels = sort(unique(
                    format(as.Date(date), "%Y-%m"))))),
    fintech_std = as.numeric(scale(pct_fintech_index)),
    period = case_when(
      t_rel %in% c(-4,-3) ~ "announcement",
      t_rel %in% c(-2,-1) ~ "transition",
      t_rel %in%  0:1     ~ "peak",
      t_rel %in%  2:6     ~ "recovery",
      t_rel >= 7           ~ "post",
      TRUE                 ~ "pre"
    ),
    period = factor(period,
      levels = c("pre","announcement","transition","peak","recovery","post"))
  )

cat("State panel:", nrow(sp), "obs |", n_distinct(sp$state), "states\n")
cat("t_rel range:", min(sp$t_rel), "to", max(sp$t_rel), "\n")
cat("t_num range:", min(sp$t_num), "to", max(sp$t_num), "\n")

# ── 2. Event study (baseline) ──────────────────────────────────────────────────
sp_es <- sp |> filter(t_rel >= -36 & t_rel <= 22)

# ref = -6 = Aug 2022, last clean pre-shock month before announcement window
mod_es <- feols(
  ln_ntl_mean ~ i(t_rel, fintech_std, ref = -6) | state + date_fct,
  data    = sp_es,
  cluster = ~state
)

# ── 3. Pre-period joint F-test (PROPER Wald test) ────────────────────────────
# Identify pre-period coefficient names (k < -4 to avoid announcement window)
all_terms   <- names(coef(mod_es))
pre_terms   <- all_terms[grepl("^t_rel::", all_terms) &
                 as.integer(gsub("t_rel::(-?[0-9]+):.*","\\1", all_terms)) < -4 &
                 as.integer(gsub("t_rel::(-?[0-9]+):.*","\\1", all_terms)) >= -36]

cat("\nPre-period terms for joint F-test:", length(pre_terms), "\n")

# Wald test via linearHypothesis equivalent in fixest
pre_wald <- wald(mod_es, keep = pre_terms)
pre_F    <- pre_wald$stat
pre_p    <- pre_wald$p
pre_df   <- length(pre_terms)

cat(sprintf("Joint pre-period Wald F(%d): %.3f  p = %.4f\n", pre_df, pre_F, pre_p))

# ── 4. State-specific linear trend robustness ─────────────────────────────────
# Adds state × t_num to absorb state-specific linear time trends
sp_es2 <- sp_es |> mutate(t_num_c = t_num - mean(t_num))   # centre for stability

mod_es_trend <- feols(
  ln_ntl_mean ~ i(t_rel, fintech_std, ref = -6) | state + date_fct + state[t_num_c],
  data    = sp_es2,
  cluster = ~state
)

# Pre-period Wald F for trend model
all_terms_tr  <- names(coef(mod_es_trend))
pre_terms_tr  <- all_terms_tr[grepl("^t_rel::", all_terms_tr) &
                   as.integer(gsub("t_rel::(-?[0-9]+):.*","\\1", all_terms_tr)) < -4 &
                   as.integer(gsub("t_rel::(-?[0-9]+):.*","\\1", all_terms_tr)) >= -36]

pre_wald_tr   <- wald(mod_es_trend, keep = pre_terms_tr)
cat(sprintf("State-trend model pre-period Wald F(%d): %.3f  p = %.4f\n",
            length(pre_terms_tr), pre_wald_tr$stat, pre_wald_tr$p))

# Compare peak coefficient with and without state trends
peak_base  <- coef(mod_es)["t_rel::0:fintech_std"]
peak_trend <- coef(mod_es_trend)["t_rel::0:fintech_std"]
cat(sprintf("Peak coef: baseline = %.4f | state-trend = %.4f\n",
            peak_base, peak_trend))

# ── 5. Extract coefficients for plotting ──────────────────────────────────────
extract_es <- function(mod) {
  cis <- as.data.frame(confint(mod, level = 0.95)) |>
    tibble::rownames_to_column("term") |>
    filter(grepl("^t_rel", term)) |>
    mutate(
      k   = as.integer(gsub("t_rel::(-?[0-9]+):.*","\\1", term)),
      est = coef(mod)[term],
      se  = se(mod)[term]
    ) |>
    arrange(k)
  # Add omitted category
  omit <- data.frame(term = "t_rel::-6:fintech_std",
                     `2.5 %` = 0, `97.5 %` = 0, k = -6L, est = 0, se = 0,
                     stringsAsFactors = FALSE, check.names = FALSE)
  bind_rows(cis, omit) |> arrange(k)
}

es_coef       <- extract_es(mod_es)
es_coef_trend <- extract_es(mod_es_trend)

# ── 6. Theme ──────────────────────────────────────────────────────────────────
theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    axis.title       = element_text(size = 10),
    plot.caption     = element_text(size = 8, hjust = 0),
    legend.position  = "none"
  )

month_labels <- function(x) {
  yr <- 2023 + (x + 2) %/% 12
  mo <- ((x + 2 - 1) %% 12) + 1
  paste0(month.abb[mo], "\n", yr)
}

# ── 7. Main event study figure ────────────────────────────────────────────────
ymax <- max(es_coef$`97.5 %`, na.rm = TRUE)

fig_es <- ggplot(es_coef, aes(x = k, y = est)) +
  annotate("rect", xmin = -4, xmax = 7, ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.06) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = -4, linetype = "dotted", color = "grey50", linewidth = 0.5) +
  geom_vline(xintercept =  0, linetype = "solid",  color = "grey30", linewidth = 0.4) +
  geom_ribbon(aes(ymin = `2.5 %`, ymax = `97.5 %`),
              fill = "steelblue", alpha = 0.20) +
  geom_line(color = "steelblue4", linewidth = 0.7) +
  geom_point(aes(size = (k == 0), shape = (k == 0)), color = "steelblue4") +
  scale_size_manual(values  = c("FALSE" = 1.8, "TRUE" = 3.5), guide = "none") +
  scale_shape_manual(values = c("FALSE" = 19,  "TRUE" = 18),  guide = "none") +
  annotate("text", x = -4.3, y = ymax * 0.95,
           label = "CBN\nannouncement", size = 2.8, hjust = 1, color = "grey30") +
  annotate("text", x =  0.3, y = ymax * 0.95,
           label = "Peak crunch\n(Feb 2023)",  size = 2.8, hjust = 0, color = "grey30") +
  scale_x_continuous(breaks = seq(-36, 22, by = 6), labels = month_labels) +
  labs(x = NULL,
       y = expression(hat(beta)[k] ~ "(log NTL differential, 1 SD fintech)")) +
  theme_paper

ggsave(file.path(fig_dir, "fig_eventstudy.pdf"),
       fig_es, width = 7, height = 3.8, device = cairo_pdf)
cat("Saved: fig_eventstudy.pdf\n")

# ── 8. State-trend robustness figure (for appendix) ───────────────────────────
es_both <- bind_rows(
  es_coef       |> mutate(spec = "Baseline TWFE"),
  es_coef_trend |> mutate(spec = "State linear trends")
)

fig_trend <- ggplot(es_both, aes(x = k, y = est, color = spec, fill = spec)) +
  annotate("rect", xmin = -4, xmax = 7, ymin = -Inf, ymax = Inf,
           fill = "grey80", alpha = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = -4, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept =  0, color = "grey30") +
  geom_ribbon(aes(ymin = `2.5 %`, ymax = `97.5 %`), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_color_manual(values = c("Baseline TWFE" = "steelblue4",
                                "State linear trends" = "firebrick3")) +
  scale_fill_manual( values = c("Baseline TWFE" = "steelblue",
                                "State linear trends" = "firebrick1")) +
  scale_x_continuous(breaks = seq(-36, 22, by = 6), labels = month_labels) +
  labs(x = NULL,
       y = expression(hat(beta)[k] ~ "(log NTL differential, 1 SD fintech)"),
       color = NULL, fill = NULL) +
  theme_paper +
  theme(legend.position = c(0.18, 0.88),
        legend.background = element_rect(fill = "white", linewidth = 0.3))

ggsave(file.path(fig_dir, "fig_eventstudy_trend.pdf"),
       fig_trend, width = 7, height = 3.8, device = cairo_pdf)
cat("Saved: fig_eventstudy_trend.pdf\n")

# ── 9. DiD collapsed: 4-period model ──────────────────────────────────────────
mod1 <- feols(
  ln_ntl_mean ~ i(period, fintech_std, ref = "pre") | state + date_fct,
  data = sp, cluster = ~state
)

# State-trend version of collapsed DiD
mod1_trend <- feols(
  ln_ntl_mean ~ i(period, fintech_std, ref = "pre") | state + date_fct + state[t_num_c],
  data = sp |> mutate(t_num_c = t_num - mean(t_num)),
  cluster = ~state
)

# WBES states subsample (19 states with 2014 WBES data)
wbes_states <- c("Abia","Abuja","Anambra","Cross river","Enugu","Gombe",
                 "Jigawa","Kaduna","Kano","Katsina","Kebbi","Kwara",
                 "Lagos","Nasarawa","Niger","Ogun","Oyo","Sokoto","Zamfara")
sp_wbes <- sp |> filter(state %in% wbes_states)

mod2 <- feols(
  ln_ntl_mean ~ i(period, fintech_std, ref = "pre") | state + date_fct,
  data = sp_wbes, cluster = ~state
)

# With pre-shock NTL baseline control
ntl_base <- sp |>
  filter(t_rel < -4) |>
  group_by(state) |>
  summarise(ntl_base = mean(ln_ntl_mean, na.rm = TRUE), .groups = "drop")

sp3 <- sp |>
  left_join(ntl_base, by = "state") |>
  mutate(ntl_base_x_post = ntl_base * as.integer(t_rel >= -4))

mod3 <- feols(
  ln_ntl_mean ~ i(period, fintech_std, ref = "pre") + ntl_base_x_post |
    state + date_fct,
  data = sp3, cluster = ~state
)

cat("\nCollapsed DiD peak coefficients:\n")
cat("  Baseline:      ", round(coef(mod1)["period::peak:fintech_std"], 4),
    "SE:", round(se(mod1)["period::peak:fintech_std"], 4), "\n")
cat("  State trends:  ", round(coef(mod1_trend)["period::peak:fintech_std"], 4),
    "SE:", round(se(mod1_trend)["period::peak:fintech_std"], 4), "\n")
cat("  WBES states:   ", round(coef(mod2)["period::peak:fintech_std"], 4), "\n")
cat("  NTL control:   ", round(coef(mod3)["period::peak:fintech_std"], 4), "\n")

# ── 10. Export DiD table ──────────────────────────────────────────────────────
coef_map <- c(
  "period::announcement:fintech_std" = "Fintech $\\times$ Announcement",
  "period::transition:fintech_std"   = "Fintech $\\times$ Transition",
  "period::peak:fintech_std"         = "Fintech $\\times$ Peak crunch",
  "period::recovery:fintech_std"     = "Fintech $\\times$ Recovery",
  "period::post:fintech_std"         = "Fintech $\\times$ Post-shock",
  "ntl_base_x_post"                  = "Baseline NTL $\\times$ Post"
)

rows_extra <- tribble(
  ~term,            ~`(1)`,  ~`(2)`,  ~`(3)`,  ~`(4)`,
  "State FE",       "Yes",   "Yes",   "Yes",   "Yes",
  "Month-Year FE",  "Yes",   "Yes",   "Yes",   "Yes",
  "State trends",   "No",    "Yes",   "No",    "No",
  "Pre-NTL ctrl.",  "No",    "No",    "No",    "Yes",
  "States",         "37",    "37",    "19",    "37"
)
attr(rows_extra, "position") <- 13:17

modelsummary(
  list("Baseline" = mod1, "State trends" = mod1_trend,
       "WBES states" = mod2, "NTL control" = mod3),
  coef_map  = coef_map,
  stars     = c("*" = .1, "**" = .05, "***" = .01),
  gof_map   = list(
    list(raw = "nobs",      clean = "Observations", fmt = 0),
    list(raw = "r.squared", clean = "$R^2$",        fmt = 3)
  ),
  add_rows  = rows_extra,
  output    = file.path(tab_dir, "tab_did_macro.tex"),
  title     = "Differential Effect of Pre-Shock Fintech Adoption on Nighttime Lights",
  notes     = paste(
    "Dependent variable: $\\ln(\\text{NTL}_{st}+1)$.",
    "Treatment is the standardised state-level fintech index from NLPS Round~6 (Oct 2022).",
    "Period definitions follow Table~1; pre-shock is the omitted category.",
    "Column (2) adds a state-specific linear time trend (state $\\times$ month index).",
    "Column (3) restricts to the 19 states with 2014 WBES coverage.",
    "Column (4) adds baseline mean NTL interacted with a post-announcement indicator.",
    "Standard errors clustered at the state level.",
    "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
  )
)
cat("Saved: tab_did_macro.tex\n")

# ── 11. Magnitude interpretation ──────────────────────────────────────────────
mean_ntl_pre  <- mean(sp$ln_ntl_mean[sp$t_rel < -4], na.rm = TRUE)
peak_coef     <- coef(mod1)["period::peak:fintech_std"]
peak_pct_diff <- (exp(peak_coef) - 1) * 100

cat(sprintf("\nMagnitude: 1 SD fintech → %.2f%% differential NTL at peak crunch\n",
            peak_pct_diff))
cat(sprintf("(Mean pre-shock ln(NTL+1) = %.4f)\n", mean_ntl_pre))

# ── 12. Save key numbers ──────────────────────────────────────────────────────
key_numbers <- list(
  # Event study
  peak_coef_es    = coef(mod_es)["t_rel::0:fintech_std"],
  peak_se_es      = se(mod_es)["t_rel::0:fintech_std"],
  # Pre-period joint F-test
  pre_F_stat      = pre_F,
  pre_F_p         = pre_p,
  pre_F_df        = pre_df,
  # State-trend pre-period F
  pre_F_trend     = pre_wald_tr$stat,
  pre_F_trend_p   = pre_wald_tr$p,
  # Collapsed DiD
  peak_coef_b1    = coef(mod1)["period::peak:fintech_std"],
  peak_se_b1      = se(mod1)["period::peak:fintech_std"],
  peak_pval_b1    = pvalue(mod1)["period::peak:fintech_std"],
  peak_coef_trend = coef(mod1_trend)["period::peak:fintech_std"],
  post_coef_b1    = coef(mod1)["period::post:fintech_std"],
  # Magnitude
  peak_pct_diff   = peak_pct_diff,
  mean_ntl_pre    = mean_ntl_pre,
  # Sample
  n_states        = n_distinct(sp$state),
  n_obs_b1        = nobs(mod1)
)

saveRDS(key_numbers, file.path(root, "data/processed/key_numbers_macro.rds"))

cat("\n=== Key numbers for paper ===\n")
cat(sprintf("Pre-period joint F(%d) = %.3f  (p = %.4f)\n",
            pre_df, pre_F, pre_p))
cat(sprintf("State-trend pre-period F = %.3f  (p = %.4f)\n",
            pre_wald_tr$stat, pre_wald_tr$p))
cat(sprintf("Peak crunch coef (baseline): %.4f  SE: %.4f  p: %.4f\n",
            key_numbers$peak_coef_b1, key_numbers$peak_se_b1,
            key_numbers$peak_pval_b1))
cat(sprintf("Peak crunch coef (state trends): %.4f\n", key_numbers$peak_coef_trend))
cat(sprintf("Implied %% NTL differential at peak: %.2f%%\n", peak_pct_diff))
cat("Script 09 complete.\n")
