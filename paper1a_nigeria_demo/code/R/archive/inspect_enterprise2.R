library(readr); library(dplyr)
root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
nlps <- file.path(root, "data/raw/lsms_isa/nlps_phone_survey")

# ── 1. NLPS Round 7 sect_13a: fix filter (values are strings not integers) ────
cat("=== NLPS R7 sect_13a: unique values of s13aq1 ===\n")
r7 <- read_csv(file.path(nlps,"p2r7_sect_a_2_5g_11b_13a_12.csv"),show_col_types=FALSE)
cat("s13aq1 unique values:", paste(unique(r7$s13aq1), collapse=", "), "\n")
cat("s13aq1 value counts:\n")
print(table(r7$s13aq1, useNA="always"))

# Check s13a_respondent too
cat("\ns13a_respondent unique:", paste(unique(r7$s13a_respondent), collapse=", "), "\n")

# Count with enterprise
n_ent <- sum(r7$s13a_respondent == 1 | r7$s13aq1 %in% c(1,"1","1. YES","YES"), na.rm=TRUE)
cat("Enterprise respondents:", n_ent, "out of", nrow(r7), "\n\n")

# ── 2. GHSP Wave 5 sect9: what are the key enterprise variables? ──────────────
cat("=== GHSP Wave 5 sect9: enterprise activity and revenue ===\n")
w5_path <- file.path(root,"data/raw/lsms_isa/wave5_ghsp",
                     "Post Harvest Wave 5/Household/sect9_harvestw5.csv")
w5 <- read_csv(w5_path, show_col_types=FALSE)

# Check what sect9 actually is: enterprise or dwelling?
cat("s9q1 unique (enterprise presence?):", paste(head(unique(w5$s9q1),8), collapse=", "), "\n")
cat("s9q2 unique (type?):", paste(head(unique(w5$s9q2),6), collapse=", "), "\n")

# Revenue/sales
if ("s9q22" %in% names(w5)) {
  cat("\ns9q22 (revenue?) - summary:\n")
  cat("  Non-NA:", sum(!is.na(w5$s9q22)), "| Range:", min(w5$s9q22,na.rm=TRUE),
      "-", max(w5$s9q22,na.rm=TRUE), "\n")
}
if ("s9q23" %in% names(w5)) {
  cat("s9q23 (profit/income?) - non-NA:", sum(!is.na(w5$s9q23)), "\n")
}
if ("s9q24" %in% names(w5)) {
  cat("s9q24 - non-NA:", sum(!is.na(w5$s9q24)), "\n")
}

# Employment in enterprise
emp_cols <- names(w5)[grepl("s9q(3[5-9]|4[0-9])", names(w5))]
cat("\nEmployment-related cols:", paste(emp_cols, collapse=", "), "\n")

# ── 3. NLPS R8 sect_14: decode the 4 climate_codes (shock types) ─────────────
cat("\n=== NLPS R8 sect_14: 4 climate_codes × shock experience ===\n")
r8_14 <- read_csv(file.path(nlps,"p2r8_sect_14.csv"), show_col_types=FALSE)
cat("Rows per climate_code:\n")
print(table(r8_14$climate_code))

cat("\ns14q1 (shock occurred?) by climate_code:\n")
print(table(r8_14$climate_code, r8_14$s14q1, useNA="always"))

cat("\ns14q2 (how affected?) unique values:", paste(unique(r8_14$s14q2), collapse=", "), "\n")

# s14q5 has 17 sub-items — likely channels/mechanisms of impact
cat("\ns14q5__1 to s14q5__6 (impact channels?) - % = 1:\n")
for (i in 1:6) {
  col <- paste0("s14q5__",i)
  if (col %in% names(r8_14)) cat(" ", col, ":", round(mean(r8_14[[col]]==1, na.rm=TRUE)*100,1), "%\n")
}

# ── 4. Enterprise rounds across all NLPS waves ────────────────────────────────
cat("\n=== Cross-round enterprise sections (sect_13 variants) ===\n")
ent_files <- list.files(nlps, pattern="sect_a.*13", full.names=TRUE)
for (f in ent_files) {
  df <- read_csv(f, show_col_types=FALSE)
  rnd <- regmatches(basename(f), regexpr("p2r\\d+", basename(f)))
  # identify enterprise column
  e_col <- names(df)[grepl("^s13.q1$|^s13aq1$|^s13bq1$", names(df))][1]
  if (!is.na(e_col)) {
    n_act <- sum(df[[e_col]] == 1, na.rm=TRUE)
    cat(rnd, basename(f), "| enterprise col:", e_col, "| active:", n_act, "/", nrow(df), "\n")
  }
}
