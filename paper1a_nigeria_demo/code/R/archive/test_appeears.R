library(httr2)
BASE <- "https://appeears.earthdatacloud.nasa.gov/api"
user <- "adetokunbo5"
pass <- "Tokunbiolabiodun@35"

cat("Testing AppEEARS login...\n")
resp <- tryCatch({
  request(paste0(BASE, "/login")) |>
    req_auth_basic(user, pass) |>
    req_timeout(60) |>
    req_error(is_error = function(r) FALSE) |>
    req_perform()
}, error = function(e) { cat("Connection error:", e$message, "\n"); NULL })

if (!is.null(resp)) {
  cat("HTTP status:", resp$status_code, "\n")
  body <- rawToChar(resp_body_raw(resp))
  cat("Response:", substr(body, 1, 400), "\n")

  if (resp$status_code == 200) {
    token <- resp_body_json(resp)$token
    cat("\nToken obtained! First 30 chars:", substr(token, 1, 30), "...\n")

    # Check available products containing VNP46
    cat("\nChecking for VNP46A3 product...\n")
    prod_resp <- request(paste0(BASE, "/product")) |>
      req_auth_bearer_token(token) |>
      req_timeout(30) |>
      req_perform()
    products <- resp_body_json(prod_resp)
    vnp46 <- Filter(function(p) grepl("VNP46", p$ProductAndVersion, ignore.case=TRUE), products)
    cat("VNP46 products found:", length(vnp46), "\n")
    for (p in vnp46) cat(" -", p$ProductAndVersion, ":", p$Description, "\n")
  }
}
