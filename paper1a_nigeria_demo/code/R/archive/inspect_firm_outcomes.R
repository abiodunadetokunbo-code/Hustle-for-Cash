# Inspect all firm outcome data sources for Paper 1a
# Maps: NLPS enterprise rounds, GHSP sect3b, and R8 sect_14 cash-crunch module

library(readr); library(dplyr)
root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")
ph   <- file.path(root, "data/raw/lsms_isa/wave5_ghsp/Post Harvest Wave 5/Household")

# ── 1. NLPS Enterprise: R5 (pre-shock), R7 (peak), R11 (long-run) ─────────────
cat("=== NLPS Enterprise Sections Across Rounds ===\n")

# R5 (Aug 2022 = pre-shock baseline) — check sect_13
r5 <- read_csv(file.path(nlps,"p2r5_sect_a_2_5_6_9a_11b_13_12.csv"), show_col_types=FALSE)
s13_cols <- names(r5)[grepl("^s13", names(r5))]
cat("R5 (Aug 2022 - pre-shock): s13 cols =", length(s13_cols),
    "| s13q1 active:", sum(r5[["s13q1"]] == 1, na.rm=TRUE), "/", nrow(r5), "\n")
if (length(s13_cols) > 0) cat("  First s13 cols:", paste(s13_cols[1:min(8,length(s13_cols))], collapse=", "), "\n")

# R7 (Feb 2023 = peak shock) — sect_13a
r7 <- read_csv(file.path(nlps,"p2r7_sect_a_2_5g_11b_13a_12.csv"), show_col_types=FALSE)
cat("\nR7 (Feb 2023 - peak shock): s13a enterprise respondents =",
    sum(r7$s13a_respondent == 1, na.rm=TRUE), "/", nrow(r7), "\n")
cat("  Industry codes (s13aq1): top 5 values:\n")
print(sort(table(r7$s13aq1), decreasing=TRUE)[1:8])
# Key enterprise variables
ent_vars <- c("s13aq1","s13aq3","s13aq4","s13aq6","s13aq7","s13aq8","s13aq9","s13aq10")
ent_vars_present <- intersect(ent_vars, names(r7))
cat("  Key enterprise vars present:", paste(ent_vars_present, collapse=", "), "\n")

# R11 (Apr 2024 = long-run) — sect_13b
r11 <- read_csv(file.path(nlps,"p2r11_sect_a_6_6d_13b_12.csv"), show_col_types=FALSE)
s13b_cols <- names(r11)[grepl("^s13b", names(r11))]
cat("\nR11 (Apr 2024 - long-run): s13b cols =", length(s13b_cols),
    "| s13bq1 active:", sum(r11[["s13bq1"]] == 1, na.rm=TRUE), "/", nrow(r11), "\n")

# ── 2. NLPS R8 sect_14: decode the 4 activity types ──────────────────────────
cat("\n=== NLPS R8 sect_14: Cash-crunch impact by activity type ===\n")
r8_14 <- read_csv(file.path(nlps,"p2r8_sect_14.csv"), show_col_types=FALSE)

# s14q1 = severity of shock (1=very severe... 5=not affected, 6=N/A?)
# % affected (s14q1 != 5 and != 6) by climate_code
for (cc in 1:4) {
  sub <- r8_14[r8_14$climate_code == cc, ]
  pct_affected <- mean(sub$s14q1 %in% 1:4, na.rm=TRUE) * 100
  cat(sprintf("  climate_code %d: %.0f%% affected (n=%d)\n", cc, pct_affected, nrow(sub)))
}

# s14q2 = direction of impact (1=increase, 2=decrease, etc.)?
cat("\ns14q2 (direction?) distribution by climate_code:\n")
print(table(r8_14$climate_code, r8_14$s14q2))

# ── 3. GHSP Wave 5 sect3b: non-farm enterprise income (Feb 2024) ──────────────
cat("\n=== GHSP Wave 5 sect3b: non-farm enterprise (Feb 2024) ===\n")
w5_3b <- read_csv(file.path(ph,"sect3b_harvestw5.csv"), show_col_types=FALSE)
cat("Rows:", nrow(w5_3b), "| Cols:", ncol(w5_3b), "\n")

# Find income/revenue variables
income_cols <- names(w5_3b)[grepl("s3bq(3|4|5|6|7|8|9|1[0-9])", names(w5_3b))]
cat("Income-related cols:", paste(income_cols[1:min(12,length(income_cols))], collapse=", "), "\n")

# Check PPMIGASP prefilled (likely enterprise income from previous visit)
if ("PPMIGASP_prefilled" %in% names(w5_3b)) {
  cat("PPMIGASP (enterprise income, prefilled):\n")
  cat("  Non-NA:", sum(!is.na(w5_3b$PPMIGASP_prefilled)),
      "| Range:", min(w5_3b$PPMIGASP_prefilled, na.rm=TRUE),
      "-", max(w5_3b$PPMIGASP_prefilled, na.rm=TRUE), "\n")
}

# s3bq3a, s3bq3b, s3bq3c - income variables?
for (col in c("s3bq3a","s3bq3b","s3bq3c_1","s3bq4","s3bq5","s3bq6")) {
  if (col %in% names(w5_3b)) {
    non_na <- sum(!is.na(w5_3b[[col]]))
    sample_vals <- paste(head(unique(w5_3b[[col]]), 4), collapse=", ")
    cat(sprintf("  %s: non-NA=%d | sample values: %s\n", col, non_na, sample_vals))
  }
}

cat("\nDone. Summary:\n")
cat("  NLPS R5 (pre-shock enterprise baseline): check s13q1\n")
cat("  NLPS R7 (peak shock, 1,642 enterprises): industry + activity vars\n")
cat("  NLPS R11 (long-run, 406 enterprises): survival/recovery\n")
cat("  NLPS R8 sect_14: 4 activity types × severity × 17 channels\n")
cat("  GHSP W5 sect3b: non-farm enterprise income, Feb 2024\n")
