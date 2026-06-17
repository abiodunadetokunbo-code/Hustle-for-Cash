# Script 14: Inference Robustness — Pairs Cluster Bootstrap + Randomization Inference
# Focus: firm-level evidence (enterprise survival and employment).
# fwildclusterboot unavailable for this R version; implements pairs bootstrap
# (state-level cluster resampling) and RI manually.
# Outputs: paper/tables/atab_inference_robustness.tex

library(readr); library(dplyr); library(fixest)

root    <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
tab_dir <- file.path(root, "paper/tables")
dir.create(tab_dir, showWarnings = FALSE)

set.seed(2024)
B_boot <- 999
B_ri   <- 4999

# ── 1. Load firm panel ────────────────────────────────────────────────────────
fp <- read_csv(
  file.path(root, "data/processed/firm_panel_r5r7r11.csv"),
  show_col_types = FALSE
) |>
  mutate(
    round   = factor(round, levels = c("R5","R7","R11")),
    state_f = factor(state),
    hh_f    = factor(hhid)
  )

fp_work <- fp |>
  filter(!is.na(n_workers) & round %in% c("R5","R7")) |>
  mutate(round = factor(round, levels = c("R5","R7")))

states <- unique(fp$state)
G      <- length(states)
cat("Firm panel:", nrow(fp), "obs |", G, "state clusters\n")

# ── 2. Observed models ────────────────────────────────────────────────────────
mod_act <- feols(
  active ~ i(round, fintech_std, ref = "R5") | hh_f + round,
  data = fp, cluster = ~state_f
)
mod_wk <- feols(
  n_workers ~ i(round, fintech_std, ref = "R5") | hh_f + round,
  data = fp_work, cluster = ~state_f
)

obs <- list(
  act_r7  = list(coef = coef(mod_act)["round::R7:fintech_std"],
                 se   = se(mod_act)["round::R7:fintech_std"],
                 p    = pvalue(mod_act)["round::R7:fintech_std"]),
  act_r11 = list(coef = coef(mod_act)["round::R11:fintech_std"],
                 se   = se(mod_act)["round::R11:fintech_std"],
                 p    = pvalue(mod_act)["round::R11:fintech_std"]),
  wk_r7   = list(coef = coef(mod_wk)["round::R7:fintech_std"],
                 se   = se(mod_wk)["round::R7:fintech_std"],
                 p    = pvalue(mod_wk)["round::R7:fintech_std"])
)

cat(sprintf("\nObserved: act_R7=%.4f (p=%.3f) | act_R11=%.4f (p=%.3f) | wk_R7=%.4f (p=%.3f)\n",
    obs$act_r7$coef, obs$act_r7$p,
    obs$act_r11$coef, obs$act_r11$p,
    obs$wk_r7$coef,  obs$wk_r7$p))

# ── 3. Pairs cluster bootstrap ────────────────────────────────────────────────
# Resample G states with replacement; re-estimate feols on stacked data.
# Assigns fresh integer cluster IDs to avoid duplicate hhid collisions.
cat(sprintf("\nRunning pairs bootstrap (B=%d)...\n", B_boot))

boot_act_r7  <- numeric(B_boot)
boot_act_r11 <- numeric(B_boot)
boot_wk_r7   <- numeric(B_boot)

for (b in seq_len(B_boot)) {
  samp <- sample(states, G, replace = TRUE)

  # Stack selected clusters, assigning synthetic hhid to avoid FE collisions
  bd <- mapply(function(s, idx) {
    d <- fp[fp$state == s, ]
    d$hh_f_boot <- paste0(idx, "_", d$hhid)
    d
  }, samp, seq_along(samp), SIMPLIFY = FALSE)
  bd <- do.call(rbind, bd)

  bd_wk <- mapply(function(s, idx) {
    d <- fp_work[fp_work$state == s, ]
    d$hh_f_boot <- paste0(idx, "_", d$hhid)
    d
  }, samp, seq_along(samp), SIMPLIFY = FALSE)
  bd_wk <- do.call(rbind, bd_wk)

  m_act <- tryCatch(
    feols(active ~ i(round, fintech_std, ref = "R5") |
            hh_f_boot + round, data = bd,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  m_wk <- tryCatch(
    feols(n_workers ~ i(round, fintech_std, ref = "R5") |
            hh_f_boot + round, data = bd_wk,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  boot_act_r7[b]  <- if (!is.null(m_act))
    coef(m_act)["round::R7:fintech_std"]  else NA_real_
  boot_act_r11[b] <- if (!is.null(m_act))
    coef(m_act)["round::R11:fintech_std"] else NA_real_
  boot_wk_r7[b]   <- if (!is.null(m_wk))
    coef(m_wk)["round::R7:fintech_std"]   else NA_real_

  if (b %% 100 == 0) cat(sprintf("  Bootstrap iteration %d/%d\n", b, B_boot))
}

# Bootstrap SE and p-values (percentile-t, imposing null by centering)
boot_se_act_r7  <- sd(boot_act_r7,  na.rm = TRUE)
boot_se_act_r11 <- sd(boot_act_r11, na.rm = TRUE)
boot_se_wk_r7   <- sd(boot_wk_r7,   na.rm = TRUE)

# Centered bootstrap distribution for p-value
p_boot_act_r7  <- mean(abs(boot_act_r7  - obs$act_r7$coef)  >=
                        abs(obs$act_r7$coef),  na.rm = TRUE)
p_boot_act_r11 <- mean(abs(boot_act_r11 - obs$act_r11$coef) >=
                        abs(obs$act_r11$coef), na.rm = TRUE)
p_boot_wk_r7   <- mean(abs(boot_wk_r7   - obs$wk_r7$coef)  >=
                        abs(obs$wk_r7$coef),   na.rm = TRUE)

cat(sprintf("\nBootstrap SEs: act_R7=%.4f | act_R11=%.4f | wk_R7=%.4f\n",
            boot_se_act_r7, boot_se_act_r11, boot_se_wk_r7))
cat(sprintf("Bootstrap p:   act_R7=%.3f | act_R11=%.3f | wk_R7=%.3f\n",
            p_boot_act_r7, p_boot_act_r11, p_boot_wk_r7))

# ── 4. Randomization Inference ────────────────────────────────────────────────
cat(sprintf("\nRunning randomization inference (permutations=%d)...\n", B_ri))

fintech_map <- fp |>
  distinct(state, fintech_std) |>
  arrange(state)

ri_act_r7  <- numeric(B_ri)
ri_act_r11 <- numeric(B_ri)
ri_wk_r7   <- numeric(B_ri)

for (i in seq_len(B_ri)) {
  perm_ft <- fintech_map |>
    mutate(fintech_perm = sample(fintech_std))

  fp_p    <- fp    |> left_join(perm_ft |> select(state, fintech_perm), by = "state")
  fp_wk_p <- fp_work |> left_join(perm_ft |> select(state, fintech_perm), by = "state")

  m_act <- tryCatch(
    feols(active ~ i(round, fintech_perm, ref = "R5") | hh_f + round,
          data = fp_p, warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  m_wk <- tryCatch(
    feols(n_workers ~ i(round, fintech_perm, ref = "R5") | hh_f + round,
          data = fp_wk_p, warn = FALSE, notes = FALSE),
    error = function(e) NULL)

  ri_act_r7[i]  <- if (!is.null(m_act))
    coef(m_act)["round::R7:fintech_perm"]  else NA_real_
  ri_act_r11[i] <- if (!is.null(m_act))
    coef(m_act)["round::R11:fintech_perm"] else NA_real_
  ri_wk_r7[i]   <- if (!is.null(m_wk))
    coef(m_wk)["round::R7:fintech_perm"]   else NA_real_

  if (i %% 1000 == 0) cat(sprintf("  RI permutation %d/%d\n", i, B_ri))
}

p_ri_act_r7  <- mean(abs(ri_act_r7)  >= abs(obs$act_r7$coef),  na.rm = TRUE)
p_ri_act_r11 <- mean(abs(ri_act_r11) >= abs(obs$act_r11$coef), na.rm = TRUE)
p_ri_wk_r7   <- mean(abs(ri_wk_r7)  >= abs(obs$wk_r7$coef),   na.rm = TRUE)

cat(sprintf("RI p-values: act_R7=%.3f | act_R11=%.3f | wk_R7=%.3f\n",
            p_ri_act_r7, p_ri_act_r11, p_ri_wk_r7))

# ── 5. Export LaTeX table ──────────────────────────────────────────────────────
star_fn <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) "***" else if (p < 0.05) "**" else if (p < 0.10) "*" else ""
}

rows <- list(
  list(label = "Enterprise active $\\times$ R7 (peak crunch)",
       coef = obs$act_r7$coef, se = obs$act_r7$se,   p_asy = obs$act_r7$p,
       boot_se = boot_se_act_r7, p_boot = p_boot_act_r7, p_ri = p_ri_act_r7),
  list(label = "Enterprise active $\\times$ R11 (medium run)",
       coef = obs$act_r11$coef, se = obs$act_r11$se, p_asy = obs$act_r11$p,
       boot_se = boot_se_act_r11, p_boot = p_boot_act_r11, p_ri = p_ri_act_r11),
  list(label = "Enterprise workers $\\times$ R7 (peak crunch)",
       coef = obs$wk_r7$coef, se = obs$wk_r7$se,     p_asy = obs$wk_r7$p,
       boot_se = boot_se_wk_r7,  p_boot = p_boot_wk_r7,  p_ri = p_ri_wk_r7)
)

tex <- c(
  "\\begin{table}[H]",
  "\\begin{threeparttable}",
  "\\caption{Firm-Level Inference Robustness: Pairs Bootstrap and Randomization Inference}",
  "\\label{atab:inference_robustness}",
  "\\small",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  paste0("Outcome $\\times$ Round & Coef. & Asy.\\ SE & $p_{\\text{asy}}$",
         " & Boot.\\ SE & $p_{\\text{boot}}$ & $p_{\\text{RI}}$ \\\\"),
  "\\midrule"
)

for (r in rows) {
  tex <- c(tex, sprintf(
    "%s & %.4f & %.4f & %.3f%s & %.4f & %.3f%s & %.3f%s \\\\",
    r$label,
    r$coef, r$se,  r$p_asy,  star_fn(r$p_asy),
    r$boot_se,
    r$p_boot, star_fn(r$p_boot),
    r$p_ri,   star_fn(r$p_ri)
  ))
}

tex <- c(tex,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}",
  "\\footnotesize",
  paste0(
    "\\item \\textit{Notes:} Each row reports inference for a focal hypothesis from the",
    " firm-level DiD specification (equation~\\ref{eq:firm}).",
    " Coefficients are from Table~\\ref{tab:firm_did}.",
    " Asy.\\ SE: heteroskedasticity-robust SE clustered at the state level ($G=37$).",
    " Boot.\\ SE: standard deviation of the coefficient across ", B_boot,
    " pairs-bootstrap replications (states resampled with replacement).",
    " $p_{\\text{boot}}$: fraction of centred bootstrap draws exceeding the observed",
    " absolute coefficient.",
    " $p_{\\text{RI}}$: two-sided randomisation-inference $p$-value from ", B_ri,
    " permutations of the fintech index across states.",
    " $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
  ),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(tex, file.path(tab_dir, "atab_inference_robustness.tex"))
cat("Saved: atab_inference_robustness.tex\n")

# ── 6. Save key numbers ───────────────────────────────────────────────────────
saveRDS(
  list(
    p_boot_act_r7   = p_boot_act_r7,
    p_boot_act_r11  = p_boot_act_r11,
    p_boot_wk_r7    = p_boot_wk_r7,
    p_ri_act_r7     = p_ri_act_r7,
    p_ri_act_r11    = p_ri_act_r11,
    p_ri_wk_r7      = p_ri_wk_r7,
    boot_se_act_r7  = boot_se_act_r7,
    boot_se_act_r11 = boot_se_act_r11,
    boot_se_wk_r7   = boot_se_wk_r7,
    B_boot = B_boot,
    B_ri   = B_ri
  ),
  file.path(root, "data/processed/key_numbers_wcb.rds")
)

cat("\n=== Inference robustness summary ===\n")
cat(sprintf("Active R7:   p_asy=%.3f | p_boot=%.3f | p_RI=%.3f\n",
            obs$act_r7$p,  p_boot_act_r7,  p_ri_act_r7))
cat(sprintf("Active R11:  p_asy=%.3f | p_boot=%.3f | p_RI=%.3f\n",
            obs$act_r11$p, p_boot_act_r11, p_ri_act_r11))
cat(sprintf("Workers R7:  p_asy=%.3f | p_boot=%.3f | p_RI=%.3f\n",
            obs$wk_r7$p,   p_boot_wk_r7,   p_ri_wk_r7))
cat("Script 14 complete.\n")
