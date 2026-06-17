library(readr)
root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
pp   <- file.path(root, "data/raw/lsms_isa/wave5_ghsp/Post Planting Wave 5/Household")
ph   <- file.path(root, "data/raw/lsms_isa/wave5_ghsp/Post Harvest Wave 5/Household")

check_file <- function(path, label) {
  if (!file.exists(path)) return(invisible(NULL))
  df <- read_csv(path, show_col_types = FALSE, n_max = 3)
  cat("\n---", label, "(", ncol(df), "cols,",
      format(file.size(path)/1e3, big.mark=","), "KB ) ---\n")
  cat(paste(names(df)[1:min(14, ncol(df))], collapse = ", "), "\n")
  biz <- names(df)[grepl("revenue|sales|profit|employ|worker|income|earn",
                           names(df), ignore.case = TRUE)]
  if (length(biz) > 0) cat("Enterprise cols:", paste(head(biz, 6), collapse=", "), "\n")
  return(names(df))
}

check_file(file.path(pp, "sect9_plantingw5.csv"),  "PP sect9  (NFE candidate)")
check_file(file.path(pp, "sect3_plantingw5.csv"),  "PP sect3  (labor/income candidate)")
check_file(file.path(ph, "sect3a_harvestw5.csv"),  "PH sect3a")
check_file(file.path(ph, "sect3b_harvestw5.csv"),  "PH sect3b")
check_file(file.path(ph, "sect3c_harvestw5.csv"),  "PH sect3c")

# Also check R8 sect_14 climate_codes more carefully
cat("\n=== NLPS R8 sect_14: what do climate_codes represent? ===\n")
r8 <- read_csv(file.path(root, "data/raw/lsms_isa/nlps_phone_survey/p2r8_sect_14.csv"),
               show_col_types = FALSE)
cat("climate_code 1 – s14q1 distribution:\n")
print(table(r8[r8$climate_code==1, "s14q1"]))
cat("\nclimate_code 2 – s14q1 distribution:\n")
print(table(r8[r8$climate_code==2, "s14q1"]))
cat("\nAll s14q5 channels – % of climate_code 1 households affected:\n")
for (i in 1:17) {
  col <- paste0("s14q5__", i)
  if (col %in% names(r8)) {
    pct <- mean(r8[r8$climate_code==1, col][[1]] == 1, na.rm = TRUE) * 100
    if (pct > 0) cat(sprintf("  %s: %.1f%%\n", col, pct))
  }
}
