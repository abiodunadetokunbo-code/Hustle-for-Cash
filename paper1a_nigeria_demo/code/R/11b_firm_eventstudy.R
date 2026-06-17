# Script 11b: Firm-Level Survival Dynamics Figure
# Plots round-specific DiD coefficients (fintech × round) on a calendar timeline.
# Outcome: enterprise active (binary). R5 (Aug 2022) = reference = 0.
# Outputs: paper/figures/fig_eventstudy_firm.pdf

library(readr); library(dplyr); library(fixest)
library(ggplot2)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
fig_dir <- file.path(root, "paper/figures")
dir.create(fig_dir, showWarnings = FALSE)

# ── 1. Load enterprise panel ──────────────────────────────────────────────────
fp <- read_csv(
  file.path(root, "data/processed/firm_panel_r5r7r11.csv"),
  show_col_types = FALSE
) |>
  mutate(
    round   = factor(round, levels = c("R5", "R7", "R11")),
    state_f = factor(state)
  )

cat("Enterprise panel:", nrow(fp), "rows |",
    n_distinct(fp$hhid), "households |",
    n_distinct(fp$state), "states\n")

# ── 2. Estimate round × fintech interactions ──────────────────────────────────
# R5 = reference. Household FE + round FE. Cluster at state level.
mod_surv <- feols(
  active ~ i(round, fintech_std, ref = "R5") | hhid + round,
  data    = fp,
  cluster = ~state_f
)

cat("\n=== Round × fintech coefficients (enterprise survival) ===\n")
print(coeftable(mod_surv))

# ── 3. Build plot data ────────────────────────────────────────────────────────
# Approximate midpoint of each fielding window
round_meta <- tibble(
  round      = c("R5",          "R7",          "R11"),
  date       = as.Date(c("2022-08-15", "2023-02-15", "2024-04-15")),
  label_text = c("R5\n(Aug 2022, baseline)",
                 "R7\n(Feb 2023, peak crunch)",
                 "R11\n(Apr 2024, medium run)")
)

# Extract CIs and rename for ggplot safety
ci_raw <- confint(mod_surv, level = 0.95) |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  rename(ci_lo = `2.5 %`, ci_hi = `97.5 %`) |>
  filter(grepl("^round::", term)) |>
  mutate(
    round = gsub("round::(.+):fintech_std", "\\1", term),
    est   = coef(mod_surv)[term],
    se    = se(mod_surv)[term]
  )

# Reference row for R5
ref_row <- tibble(
  term  = "round::R5:fintech_std",
  ci_lo = 0, ci_hi = 0,
  round = "R5", est = 0, se = 0
)

plot_df <- bind_rows(ref_row, ci_raw) |>
  left_join(round_meta, by = "round") |>
  mutate(
    is_ref = (round == "R5"),
    est_pp = est * 100,
    ci_lo_pp = ci_lo * 100,
    ci_hi_pp = ci_hi * 100
  )

cat("\nPlot data:\n")
print(plot_df[, c("round", "date", "est_pp", "ci_lo_pp", "ci_hi_pp")])

# ── 4. Key event dates ────────────────────────────────────────────────────────
cbn_ann    <- as.Date("2022-10-26")  # CBN announcement
sup_court  <- as.Date("2023-02-08")  # Supreme Court ruling
crunch_end <- as.Date("2023-07-01")  # Cash normalisation (approx)
r6_date    <- as.Date("2022-10-15")  # NLPS R6: fintech measurement date

# ── 5. Paper theme ────────────────────────────────────────────────────────────
theme_paper <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor  = element_blank(),
    panel.grid.major  = element_line(color = "grey92"),
    axis.title        = element_text(size = 10),
    axis.text.x       = element_text(size = 8.5),
    plot.caption      = element_text(size = 7.5, hjust = 0),
    legend.position   = "none"
  )

# ── 6. Figure ─────────────────────────────────────────────────────────────────
# y-range for label placement
y_top <- max(plot_df$ci_hi_pp, na.rm = TRUE) * 1.12
y_ann <- max(plot_df$ci_hi_pp, na.rm = TRUE) * 0.85

fig_firm <- ggplot(plot_df, aes(x = date, y = est_pp)) +

  # Crunch period shading: CBN announcement → cash normalisation
  annotate("rect",
           xmin = cbn_ann, xmax = crunch_end,
           ymin = -Inf, ymax = Inf,
           fill = "steelblue", alpha = 0.07) +

  # Zero reference
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey40", linewidth = 0.4) +

  # Key event lines
  geom_vline(xintercept = cbn_ann, linetype = "dotted",
             color = "grey45", linewidth = 0.55) +
  geom_vline(xintercept = sup_court, linetype = "dashed",
             color = "grey45", linewidth = 0.45) +

  # 95% CI whiskers
  geom_linerange(aes(ymin = ci_lo_pp, ymax = ci_hi_pp),
                 color = "steelblue3", linewidth = 1.1,
                 na.rm = TRUE) +

  # Point estimates: hollow circle for reference, solid for estimated
  geom_point(aes(shape = is_ref, size = is_ref),
             color = "steelblue4", fill = "white") +
  scale_shape_manual(values = c("TRUE" = 1, "FALSE" = 19), guide = "none") +
  scale_size_manual( values = c("TRUE" = 3,  "FALSE" = 3.8), guide = "none") +

  # Survey round labels
  geom_text(aes(label = label_text),
            vjust = -0.6, size = 2.55, color = "grey25",
            lineheight = 0.85) +

  # Event annotations
  annotate("text", x = cbn_ann + 3, y = y_ann,
           label = "CBN\nannouncement\n(26 Oct 2022)",
           size = 2.45, hjust = 0, color = "grey35", lineheight = 0.85) +
  annotate("text", x = sup_court + 3, y = y_ann * 0.65,
           label = "Supreme Court\nruling\n(8 Feb 2023)",
           size = 2.45, hjust = 0, color = "grey35", lineheight = 0.85) +

  # Axes
  scale_x_date(
    breaks = as.Date(c("2022-07-01","2022-10-01","2023-01-01",
                       "2023-07-01","2024-01-01","2024-07-01")),
    date_labels = "%b\n%Y",
    limits = c(as.Date("2022-05-01"), as.Date("2024-08-01"))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, " pp")
  ) +

  labs(
    x = NULL,
    y = "Differential enterprise survival\n(pp per 1 SD fintech)"
  ) +
  theme_paper

ggsave(
  file.path(fig_dir, "fig_eventstudy_firm.pdf"),
  fig_firm, width = 6.8, height = 3.8,
  device = cairo_pdf
)
cat("\nSaved: paper/figures/fig_eventstudy_firm.pdf\n")

# ── 7. Save key numbers for paper ─────────────────────────────────────────────
saveRDS(
  list(
    r7_est_pp  = plot_df$est_pp[plot_df$round == "R7"],
    r7_ci_lo   = plot_df$ci_lo_pp[plot_df$round == "R7"],
    r7_ci_hi   = plot_df$ci_hi_pp[plot_df$round == "R7"],
    r11_est_pp = plot_df$est_pp[plot_df$round == "R11"],
    r11_ci_lo  = plot_df$ci_lo_pp[plot_df$round == "R11"],
    r11_ci_hi  = plot_df$ci_hi_pp[plot_df$round == "R11"]
  ),
  file.path(root, "data/processed/key_numbers_firm_eventstudy.rds")
)
cat("Script 11b complete.\n")
