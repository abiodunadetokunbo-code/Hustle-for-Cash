# Test: download VNP46A3 tile using .netrc Basic Auth (bypasses bearer token EULA)
# This approach uses username/password directly in the HTTP request

library(httr2)
library(terra)

root <- "C:/Users/Admin/Desktop/Micro4Macro/paper1a_nigeria_demo"
dest <- file.path(root, "data/raw/viirs_tiles/test_tile_basic.h5")

# Credentials
user <- "adetokunbo5"
pass <- "Tokunbiolabiodun@35"

url <- "https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5200/VNP46A3/2019/001/VNP46A3.A2019001.h18v07.002.2025133151707.h5"

cat("Trying Basic Auth download...\n")
resp <- tryCatch({
  request(url) |>
    req_auth_basic(user, pass) |>
    req_timeout(120) |>
    req_perform()
}, error = function(e) { cat("Error:", e$message, "\n"); NULL })

if (!is.null(resp)) {
  body <- resp_body_raw(resp)
  cat("Response size:", length(body), "bytes\n")
  cat("First 8 bytes (HDF5 magic bytes should be: 89 48 44 46 0d 0a 1a 0a):\n")
  cat(paste(sprintf("%02X", as.integer(body[1:8])), collapse=" "), "\n")

  writeBin(body, dest)

  if (length(body) > 1000000) {
    cat("File looks like real HDF5 (>1MB). Testing rast()...\n")
    r <- tryCatch(rast(dest), error = function(e) { cat("rast FAIL:", e$message, "\n"); NULL })
    if (!is.null(r)) cat("SUCCESS - rast() worked!\n")
    r2 <- tryCatch(sds(dest), error = function(e) { cat("sds FAIL:", e$message, "\n"); NULL })
    if (!is.null(r2)) { cat("SUCCESS - sds():\n"); print(r2) }
  } else {
    # Check if it's HTML (login redirect)
    txt <- rawToChar(body[1:min(200, length(body))])
    cat("Content preview:", txt, "\n")
  }
} else {
  cat("Request failed\n")
}
