library(terra)
library(httr2)

# Read token from the main script
lines <- readLines("code/R/04_viirs_monthly_blackmarbler.R")
token_line <- lines[grepl("^NASA_TOKEN", lines)][1]
token <- gsub('NASA_TOKEN <- "', "", token_line, fixed=TRUE)
token <- gsub('"', "", token, fixed=TRUE)
token <- trimws(token)
cat("Token length:", nchar(token), "\n")

# Download test tile
url  <- "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5200/VNP46A3/2019/001/VNP46A3.A2019001.h18v07.002.2025133151707.h5"
dest <- "data/raw/viirs_tiles/test_tile.h5"

if (!file.exists(dest)) {
  cat("Downloading test tile...\n")
  resp <- request(url) |>
    req_headers(Authorization = paste("Bearer", token)) |>
    req_timeout(120) |>
    req_perform()
  writeBin(resp_body_raw(resp), dest)
}
cat("Tile size:", round(file.size(dest)/1e6, 2), "MB\n")

# Approach 1: terra::rast()
cat("\n--- Approach 1: terra::rast() ---\n")
r1 <- tryCatch(rast(dest), error = function(e) { cat("FAIL:", e$message, "\n"); NULL })
if (!is.null(r1)) { cat("SUCCESS\n"); print(r1) }

# Approach 2: terra::sds()
cat("\n--- Approach 2: terra::sds() ---\n")
s <- tryCatch(sds(dest), error = function(e) { cat("FAIL:", e$message, "\n"); NULL })
if (!is.null(s)) { cat("SUCCESS - subdatasets found:\n"); print(s) }

# Approach 3: Explicit HDF5 subdataset path
cat("\n--- Approach 3: Explicit subdataset ---\n")
p <- paste0('HDF5:"', gsub("\\\\", "/", normalizePath(dest)), '"://HDFEOS/GRIDS/VNP_Grid_DNB/Data_Fields/NearNadir_Composite_Snow_Free')
cat("Path:", p, "\n")
r3 <- tryCatch(rast(p), error = function(e) { cat("FAIL:", e$message, "\n"); NULL })
if (!is.null(r3)) { cat("SUCCESS\n"); print(r3) }

# Approach 4: hdf5r if available
cat("\n--- Approach 4: hdf5r ---\n")
if (requireNamespace("hdf5r", quietly = TRUE)) {
  library(hdf5r)
  f <- H5File$new(dest, mode = "r")
  cat("File opened. Root groups:", ls(f), "\n")
  f$close_all()
} else {
  cat("hdf5r not installed. Trying to install...\n")
  install.packages("hdf5r", repos = "https://cloud.r-project.org", quiet = TRUE)
  if (requireNamespace("hdf5r", quietly = TRUE)) {
    library(hdf5r)
    f <- H5File$new(dest, mode = "r")
    cat("File opened. Root groups:", ls(f), "\n")
    f$close_all()
  } else {
    cat("hdf5r install failed\n")
  }
}
