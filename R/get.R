#' GET Accounts
get_accounts <- function(token) {
  assertthat::assert_that(
    has_scope(token, "accounts"),
    msg = "Token needs scope: accounts."
  )
  get(token, "/accounts/list")
}

#' GET Transactions
get_transactions <- function(token) {
  assertthat::assert_that(
    has_scope(token, "transactions"),
    msg = "Token needs scope: transactions."
  )
  get(token, "/transactions")
}

#' GET Investments
get_investments <- function(token) {
  assertthat::assert_that(
    has_scope(token, "investments"),
    msg = "Token needs scope: investments."
  )
  get(token, "/investments")
}

#' General GET function
#'
#' A general function to GET all sorts of paths.
get <- function(token, path) {

  get_config <- list(
    httr::accept_json(),
    httr::user_agent("https://github.com/jacobferlin/tinkr"),
    httr::add_headers(
      Authorization = auth_str(token),
      Charset = "UTF-8")
  )

  # GET
  resp <- httr::GET(
    url = paste0(url_base(), path),
    httr::accept_json(),
    httr::user_agent("https://github.com/jacobferlin/tinkr"),
    httr::add_headers(
      Authorization = auth_str(token),
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

#' Base URL
#'
#' Base URL to TINK API.
url_base <- function() {
  "https://api.tink.se/api/v1"
}

#' Authentication String
#'
#' Authentication string for GET header.
auth_str <- function(token) {
  token_type   <- stringr::str_to_title(token$credentials$token_type)
  access_token <- token$credentials$access_token
  paste(token_type, access_token)
}

#' Custom Print
print.tink <- function(x, ...) {
  cat("<Tink: ", x$path, ">\n", sep = "")
  invisible(x)
}


