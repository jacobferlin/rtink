get_token <- function() {

  # Build endpoint
  endpoint <- httr::oauth_endpoint(
    authorize = "https://oauth.tink.com/0.4/authorize/",
    access    = "https://api.tink.se/api/v1/oauth/token"
  )

  # Build app
  app <- httr::oauth_app(
    "tink",
    client_id(),
    client_secret()
  )

  # Retrive token
  token <- httr::oauth2.0_token(
    endpoint = endpoint,
    app = app,
    scope = "accounts:read",
    use_oob = TRUE,
    oob_value = "http://localhost:3000/callback",
    as_header = TRUE,
    cache = TRUE,
    client_credentials = FALSE,
    query_authorize_extra = list(market = "SE")
  )

  # Assert Success
  assertthat::assert_that(
    httr::status_code(r) == 200,
    msg = paste("No success, status code", httr::status_code(r))
  )

  token
}


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

get_accounts <- function(token) {
  get(token, "accounts/list")
}

print.tink_api <- function(x, ...) {
  cat("<Tink ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}


url_base <- function() {
  "https://api.tink.se/api/v1/"
}

client_id <- function() {
  id <- Sys.getenv("TINK_CLIENT_ID")
  if (identical(id, "")) {
    stop("Please set env var TINK_CLIENT_ID to your client id",
         call. = FALSE)
  }

  id
}

client_secret <- function() {
  secret <- Sys.getenv("TINK_CLIENT_SECRET")
  if (identical(secret, "")) {
    stop("Please set env var TINK_CLIENT_SECRET to your client secret",
         call. = FALSE)
  }

  secret
}
