# Script 15: Pre-Trend Sensitivity Analysis
# Implements manual sensitivity analysis in the spirit of Rambachandran & Roth (2023).
# Since HonestDiD is unavailable for this R version, we:
#   (1) Estimate the pre-period trend gradient by regressing pre-period coefficients on k
#   (2) Extrapolate that trend into the post-period and show the "excess" treatment effect
#   (3) Compute M* — how large a per-period trend violation is needed to overturn results
#   (4) Produce a sensitivity figure for the appendix
# Outputs: paper/figures/fig_pretrend_sensitivity.pdf
#          paper/tables/atab_pretrend_sensitivity.tex

library(readr); library(dplyr); library(fixest); library(ggplot2); library(patchwork)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
fig_dir <- file.path(root, "paper/figures")
tab_dir <- file.path(root, "paper/tables")
dir.create(fig_dir, showWarnings = FALSE)
dir.create(tab_dir, showWarnings = FALSE)

# ── 1. Load state panel and re-estimate event study ───────────────────────────
sp <- read_csv(
  file.path(root, "data/processed/analysis_panel_state.csv"),
  show_col_types = FALSE
) |>
  mutate(
    date_fct    = factor(format(as.Date(date), "%Y-%m")),
    fintech_std = as.numeric(scale(pct_fintech_index)),
    state_f     = factor(state)
  )

sp_es <- sp |> filter(t_rel >= -36 & t_rel <= 22)

mod_es <- feols(
  ln_ntl_mean ~ i(t_rel, fintech_std, ref = -6) | state_f + date_fct,
  data = sp_es, cluster = ~state_f
)

# ── 2. Extract pre- and post-period coefficients ──────────────────────────────
all_terms <- names(coef(mod_es))

es_df <- data.frame(
  term = all_terms[grepl("^t_rel", all_terms)],
  stringsAsFactors = FALSE
) |>
  mutate(
    k   = as.integer(gsub("t_rel::(-?[0-9]+):.*", "\\1", term)),
    est = coef(mod_es)[term],
    se  = se(mod_es)[term],
    ci_lo = est - 1.96 * se,
    ci_hi = est + 1.96 * se
  ) |>
  bind_rows(data.frame(term = "ref", k = -6L, est = 0, se = 0,
                       ci_lo = 0, ci_hi = 0)) |>
  arrange(k)

pre_df  <- es_df |> filter(k < -4 & k >= -36)   # genuine pre-period
post_df <- es_df |> filter(k >= 0)                # post-shock

cat("Pre-period obs:", nrow(pre_df), "| Post-period obs:", nrow(post_df), "\n")

# ── 3. Estimate pre-period linear trend ───────────────────────────────────────
# Regress estimated pre-period betas on k to extract gradient
trend_fit <- lm(est ~ k, data = pre_df, weights = 1 / (se^2 + 1e-8))
trend_slope <- coef(trend_fit)["k"]
trend_intercept <- coef(trend_fit)["(Intercept)"]

cat(sprintf("\nPre-period trend: slope = %.5f per month (t = %.2f)\n",
            trend_slope,
            trend_slope / summary(trend_fit)$coefficients["k","Std. Error"]))

# ── 4. Extrapolated trend line through full event window ──────────────────────
es_df <- es_df |>
  mutate(
    trend_extrap = trend_intercept + trend_slope * k,
    excess = est - trend_extrap     # treatment effect net of extrapolated trend
  )

# Summarise post-period excess effects
post_excess <- es_df |> filter(k >= 0) |>
  mutate(
    excess_se   = se,   # SE of excess same as SE of raw coef (trend is fixed)
    excess_ci_lo = excess - 1.96 * excess_se,
    excess_ci_hi = excess + 1.96 * excess_se,
    p_excess     = 2 * pnorm(-abs(excess / excess_se))
  )

cat("\nTrend-adjusted post-period effects:\n")
print(post_excess |> select(k, est, trend_extrap, excess, p_excess))

# ── 5. Compute M* — breakeven trend violation ──────────────────────────────────
# Under the smoothness restriction (consecutive changes in trend ≤ M per period),
# the adjustment to the treatment effect at period k is approximately:
#   adj(k) = M * max(k - k_ref, 0) for the pure bias case
# We ask: what M makes the lower CI of the peak coefficient touch zero?
#
# Simple approximation (linear accumulation of trend violation):
#   lower_bound(peak) = est_peak - 1.96*se_peak - M * |k_peak - k_ref|
#   Set lower_bound = 0 => M* = (est_peak - 1.96*se_peak) / |k_peak - k_ref|

peak_row   <- es_df |> filter(k == 0)
k_ref      <- -6
k_peak     <- 0
distance   <- abs(k_peak - k_ref)  # 6 months

est_peak   <- peak_row$est
se_peak    <- peak_row$se
ci_lo_peak <- est_peak - 1.96 * se_peak

# Breakeven M
M_star <- (est_peak - 1.96 * se_peak) / distance
M_obs  <- abs(trend_slope)   # observed pre-period gradient magnitude

cat(sprintf("\nPeak estimate: %.5f  SE: %.5f  95%% CI: [%.5f, %.5f]\n",
            est_peak, se_peak, ci_lo_peak, est_peak + 1.96 * se_peak))
cat(sprintf("Distance from ref to peak: %d months\n", distance))
cat(sprintf("M* (breakeven trend violation per month): %.5f\n", M_star))
cat(sprintf("M_obs (observed pre-period gradient): %.5f\n", M_obs))
cat(sprintf("Ratio M*/M_obs: %.2f  (>1 means result survives at observed trend magnitude)\n",
            M_star / M_obs))

# Sensitivity table: for a range of M values, show lower and upper bounds on peak
M_vals <- seq(0, 4 * abs(M_obs), length.out = 20)

sens_df <- data.frame(
  M         = M_vals,
  lb_peak   = est_peak - 1.96 * se_peak - M_vals * distance,
  ub_peak   = est_peak + 1.96 * se_peak + M_vals * distance,
  lb_r11    = NA_real_,
  ub_r11    = NA_real_
)

# Also do for period k=14 (approximately R11 month, Apr 2024)
# Find closest available post-period coefficient
r11_row  <- es_df |> filter(k >= 14) |> slice(1)
k_r11    <- r11_row$k
est_r11  <- r11_row$est
se_r11   <- r11_row$se
dist_r11 <- abs(k_r11 - k_ref)

sens_df$lb_r11 <- est_r11 - 1.96 * se_r11 - M_vals * dist_r11
sens_df$ub_r11 <- est_r11 + 1.96 * se_r11 + M_vals * dist_r11

cat(sprintf("\nMedium-run (k=%d, dist=%d): est=%.5f  SE=%.5f\n",
            k_r11, dist_r11, est_r11, se_r11))
cat(sprintf("M* for medium-run: %.5f\n",
            (est_r11 - 1.96 * se_r11) / dist_r11))

# ── 6. Figures ────────────────────────────────────────────────────────────────
theme_paper <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        axis.title = element_text(size = 10),
        legend.position = "none")

month_labels <- function(x) {
  yr <- 2023 + (x + 2) %/% 12
  mo <- ((x + 2 - 1) %% 12) + 1
  paste0(month.abb[mo], "\n", yr)
}

# Panel A: event study with extrapolated trend
p_a <- ggplot(es_df, aes(x = k, y = est)) +
  annotate("rect", xmin = -4, xmax = 7, ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.06) +
  geom_hline(yintercept = 0,  linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = -4, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept =  0, color = "grey30", linewidth = 0.4) +
  # Extrapolated trend line
  geom_line(aes(y = trend_extrap), color = "firebrick3", linetype = "dashed",
            linewidth = 0.8, na.rm = TRUE) +
  # CI ribbon
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), fill = "steelblue", alpha = 0.20) +
  geom_line(color = "steelblue4", linewidth = 0.7) +
  geom_point(aes(size = (k == 0), shape = (k == 0)), color = "steelblue4") +
  scale_size_manual(values  = c("FALSE" = 1.8, "TRUE" = 3.5), guide = "none") +
  scale_shape_manual(values = c("FALSE" = 19,  "TRUE" = 18),  guide = "none") +
  annotate("text", x = -36, y = max(es_df$ci_hi, na.rm=TRUE) * 0.85,
           label = "--- Extrapolated\n    pre-trend",
           color = "firebrick3", size = 2.8, hjust = 0) +
  scale_x_continuous(breaks = seq(-36, 22, by = 6), labels = month_labels) +
  labs(x = NULL,
       y = expression(hat(beta)[k] ~ "(log NTL, 1 SD fintech)"),
       title = "A. Event study with extrapolated pre-trend") +
  theme_paper

# Panel B: sensitivity bounds on peak effect as M grows
p_b <- ggplot(sens_df, aes(x = M)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_ribbon(aes(ymin = lb_peak, ymax = ub_peak),
              fill = "steelblue", alpha = 0.30) +
  geom_line(aes(y = lb_peak), color = "steelblue4") +
  geom_line(aes(y = ub_peak), color = "steelblue4") +
  geom_vline(xintercept = M_obs, linetype = "dotted", color = "firebrick3") +
  geom_vline(xintercept = M_star, linetype = "solid", color = "firebrick3",
             linewidth = 0.8) +
  annotate("text", x = M_obs * 1.05, y = max(sens_df$ub_peak) * 0.85,
           label = "Observed\npre-trend", color = "firebrick3",
           size = 2.6, hjust = 0) +
  annotate("text", x = M_star * 1.05, y = max(sens_df$ub_peak) * 0.65,
           label = sprintf("M* = %.4f", M_star),
           color = "firebrick3", size = 2.6, hjust = 0) +
  labs(x = "M (max per-period trend violation allowed)",
       y = "Honest CI on peak effect",
       title = "B. Sensitivity bounds at peak crunch (k = 0)") +
  theme_paper

fig_sens <- p_a / p_b

ggsave(file.path(fig_dir, "fig_pretrend_sensitivity.pdf"),
       fig_sens, width = 7, height = 7, device = cairo_pdf)
cat("Saved: fig_pretrend_sensitivity.pdf\n")

# ── 7. Export sensitivity table ───────────────────────────────────────────────
sens_tab <- sens_df |>
  filter(M %in% M_vals[c(1, 4, 7, 10, 13, 16, 20)]) |>
  mutate(
    M_label   = sprintf("%.4f", M),
    peak_ci   = sprintf("[%.4f, %.4f]", lb_peak, ub_peak),
    r11_ci    = sprintf("[%.4f, %.4f]", lb_r11, ub_r11),
    sig_peak  = if_else(lb_peak > 0, "Yes", "No"),
    sig_r11   = if_else(lb_r11 > 0, "Yes", "No")
  )

tex <- c(
  "\\begin{table}[H]",
  "\\begin{threeparttable}",
  paste0("\\caption{Pre-Trend Sensitivity: Honest Confidence Intervals for the",
         " NTL Treatment Effect}"),
  "\\label{atab:pretrend_sensitivity}",
  "\\small",
  "\\begin{tabular}{lllcc}",
  "\\toprule",
  paste0("$M$ & 95\\% CI on $\\hat{\\beta}_{\\text{peak}}$ &",
         " 95\\% CI on $\\hat{\\beta}_{k=14}$ &",
         " Peak $>0$ & $k{=}14$ $>0$ \\\\"),
  "\\midrule"
)

for (i in seq_len(nrow(sens_tab))) {
  r <- sens_tab[i, ]
  note <- if (abs(as.numeric(r$M) - M_obs) < 1e-4) " $\\leftarrow$ obs.\\ gradient" else ""
  note2 <- if (abs(as.numeric(r$M) - M_star) < 1e-3) " $\\leftarrow$ $M^*$" else ""
  tex <- c(tex, sprintf(
    "%s%s%s & %s & %s & %s & %s \\\\",
    r$M_label, note, note2,
    r$peak_ci, r$r11_ci, r$sig_peak, r$sig_r11
  ))
}

tex <- c(tex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Sensitivity analysis in the spirit of",
    " \\citet{rambachandran2023pretrends}.",
    " $M$ is the maximum allowed per-period (monthly) deviation from parallel trends.",
    " For each $M$, the honest confidence interval is computed as",
    " $[\\hat{\\beta}_k - 1.96 \\hat{\\sigma}_k - M \\cdot |k - k_{\\text{ref}}|,\\;",
    "\\hat{\\beta}_k + 1.96 \\hat{\\sigma}_k + M \\cdot |k - k_{\\text{ref}}|]$,",
    " where $k_{\\text{ref}} = -6$ (August 2022, the omitted reference period).",
    sprintf(" The observed pre-period gradient is $M_{\\text{obs}} = %.4f$.", M_obs),
    sprintf(" $M^* = %.4f$ is the violation required to make the lower bound exactly zero", M_star),
    " at the peak crunch period.",
    sprintf(" The ratio $M^*/M_{\\text{obs}} = %.2f$ indicates the pre-trend gradient",
            M_star / M_obs),
    " would need to be amplified by this factor to overturn the NTL result.",
    " The firm-level pre-trend is validated separately via the NTL pre-period window;",
    " household and enterprise fixed effects in the firm panel absorb all time-invariant",
    " heterogeneity, making the firm-level estimates more robust to this concern."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex, file.path(tab_dir, "atab_pretrend_sensitivity.tex"))
cat("Saved: atab_pretrend_sensitivity.tex\n")

# ── 8. Save key numbers ───────────────────────────────────────────────────────
saveRDS(
  list(
    trend_slope  = trend_slope,
    M_obs        = M_obs,
    M_star       = M_star,
    M_ratio      = M_star / M_obs,
    est_peak     = est_peak,
    se_peak      = se_peak,
    ci_lo_peak   = ci_lo_peak,
    k_ref        = k_ref,
    k_peak       = k_peak,
    distance     = distance,
    excess_peak  = post_excess$excess[post_excess$k == 0]
  ),
  file.path(root, "data/processed/key_numbers_honestdid.rds")
)

cat("\n=== Pre-trend sensitivity summary ===\n")
cat(sprintf("Observed pre-period gradient:    M_obs = %.5f\n", M_obs))
cat(sprintf("Breakeven trend violation:       M*    = %.5f\n", M_star))
cat(sprintf("M*/M_obs ratio:                         %.2f\n",  M_star/M_obs))
cat(sprintf("Peak NTL effect (trend-adjusted): %.5f\n",
            post_excess$excess[post_excess$k == 0]))
cat("Script 15 complete.\n")
