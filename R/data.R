get <- function(token, path) {

  # Build url
  url <- paste0(url_base(), path)

  # Build header
  token_type   <- stringr::str_to_title(token$credentials$token_type)
  access_token <- token$credentials$access_token
  auth_str     <- paste(token_type, access_token)

  # GET
  r <- httr::GET(
    url = url,
    httr::add_headers(Authorization = auth_str)
  )

  # Assert Success
  assertthat::assert_that(
    httr::status_code(r) == 200,
    msg = paste("No success, status code", httr::status_code(r))
  )

  # Parse JSON
  parsed <- jsonlite::fromJSON(httr::content(r, "text"), simplifyVector = FALSE)

  structure(
    list(
      content = parsed,
      path = path,
      response = r
    ),
    class = "tink_api"
  )
}

accounts <- function(token) {
  get(token, "/accounts/list")
}

transactions <- function(token) {
  get(token, "/transactions")
}

print.tink_api <- function(x, ...) {
  cat("<Tink ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}
