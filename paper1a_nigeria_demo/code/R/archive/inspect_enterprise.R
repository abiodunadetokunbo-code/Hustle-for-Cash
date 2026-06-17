library(readr)
library(dplyr)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")

# ── 1. NLPS R7 sect_13a — enterprise module (Feb 2023, peak shock) ────────────
cat("=== NLPS Round 7 (Feb 2023 — peak shock): sect_13a enterprise ===\n")
r7 <- read_csv(file.path(nlps, "p2r7_sect_a_2_5g_11b_13a_12.csv"),
               show_col_types = FALSE)

s13a_cols <- names(r7)[grepl("^s13a", names(r7))]
cat("Enterprise (s13a) columns:", length(s13a_cols), "\n")
cat(paste(s13a_cols, collapse = ", "), "\n\n")

# Sample enterprise respondents
cat("Households reporting enterprise activity (s13aq1=1):\n")
active <- r7 |> filter(s13aq1 == 1)
cat("  Count:", nrow(active), "out of", nrow(r7), "households\n")
if (nrow(active) > 0 & length(s13a_cols) >= 6) {
  print(active |> select(hhid, state, s13a_cols[1:min(8, length(s13a_cols))]) |> head(5))
}

# ── 2. Check which rounds have enterprise sections ───────────────────────────
cat("\n=== Enterprise sections across rounds ===\n")
rounds_with_13 <- list.files(nlps, pattern = "sect_a.*13.*\\.csv$")
cat("Rounds with sect_13:", paste(rounds_with_13, collapse = "\n  "), "\n")

# ── 3. GHSP Wave 5 sect9 — non-farm enterprise (Post-Harvest, Feb 2024) ───────
cat("\n=== GHSP Wave 5 sect9 (Post-Harvest, Feb 2024): non-farm enterprise ===\n")
ghsp_path <- file.path(root, "data/raw/lsms_isa/wave5_ghsp",
                       "Post Harvest Wave 5/Household/sect9_harvestw5.csv")
if (file.exists(ghsp_path)) {
  w5 <- read_csv(ghsp_path, show_col_types = FALSE)
  cat("Rows:", nrow(w5), "| Cols:", ncol(w5), "\n")
  cat("First columns:", paste(names(w5)[1:15], collapse = ", "), "\n")
  # Find income/revenue variables
  rev_cols <- names(w5)[grepl("rev|sal|inc|profit|earn|s9q2[0-9]|s9q3[0-9]",
                               names(w5), ignore.case = TRUE)]
  cat("Revenue/income columns:", paste(rev_cols, collapse = ", "), "\n")
}

# ── 4. Check NLPS R8 sect_14 — the dedicated cash-crunch module ──────────────
cat("\n=== NLPS Round 8 (Apr 2023) sect_14 — dedicated cash crunch module ===\n")
r8_14 <- read_csv(file.path(nlps, "p2r8_sect_14.csv"), show_col_types = FALSE)
cat("Rows:", nrow(r8_14), "| Cols:", ncol(r8_14), "\n")
cat("Columns:", paste(names(r8_14), collapse = ", "), "\n\n")
cat("Unique climate_codes:", paste(unique(r8_14$climate_code), collapse = ", "), "\n")
cat("Sample (first 3 rows):\n")
print(head(r8_14, 3))
