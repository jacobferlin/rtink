get <- function(token, path) {

  # Build url
  url <- paste0(url_base(), path)

  # Build header
  token_type   <- stringr::str_to_title(token$credentials$token_type)
  access_token <- token$credentials$access_token
  auth_str     <- paste(token_type, access_token)

  # GET
  resp <- httr::GET(
    url = url,
    httr::accept_json(),
    httr::user_agent("https://github.com/jacobferlin/rtink"),
    httr::add_headers(
      Authorization = auth_str,
      Charset = "UTF-8")
  )

  # Turn errors into R errors
  if (httr::http_error(resp)) {
    stop(
      paste0("Tink API request failed ", httr::status_code(resp)),
      call. = FALSE
    )
  }

  # Assert content-type is json
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  # Parse JSON
  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE,
    encoding = "UTF-8")

  structure(
    list(
      path = path,
      response = resp,
      parsed = parsed
    ),
    class = "tink"
  )
}

get_user <- function(token) {
  assertthat::assert_that(
    has_scope(token, "user"),
    msg = "Token needs scope: user."
  )
  get(token, "/user")
}

# TROR JAG INTE BEHÖVER DENNA, SER UT SOM DET TINK LINK GÖR!
get_credentials <- function(token) {
  assertthat::assert_that(
    has_scope(token, "credentials"),
    msg = "Token needs scope: credentials."
  )
  get(token, "/user")
}

get_accounts <- function(token) {
  assertthat::assert_that(
    has_scope(token, "accounts"),
    msg = "Token needs scope: accounts."
  )
  get(token, "/accounts/list")
}

get_transactions <- function(token) {
  assertthat::assert_that(
    has_scope(token, "transactions"),
    msg = "Token needs scope: transactions."
  )
  get(token, "/transactions")
}

get_investments <- function(token) {
  assertthat::assert_that(
    has_scope(token, "investments"),
    msg = "Token needs scope: investments."
  )
  get(token, "/investments")
}

print.tink <- function(x, ...) {
  cat("<Tink: ", x$path, ">\n", sep = "")
  invisible(x)
}

url_base <- function() {
  "https://api.tink.se/api/v1"
}
