token <- function(new_token = FALSE,
                  cache_file = ".httr-oauth") {

  if (!new_token) {
    return(token_cache(cache_file))
  }

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

token_cache <- function(filename) {

  # Check file exist
  if (!file.exists(filename)) {
    stop(
      paste0("'", filename, "' does not exists"),
      call. = FALSE
    )
  }

  # Check file is readable by readRDS[[1]]
  token <- tryCatch(
    readRDS(filename)[[1]],
    error = function(cond) {
      stop(
        paste0("'", filename, "' could not be read"),
        call. = FALSE
      )
    }
  )

  # Check class
  if (!inherits(token, "Token2.0")) {
    stop(
      paste0("'", filename, "' does not contain a token"),
      call. = FALSE
    )
  }

  token
}

url_base <- function() {
  "https://api.tink.se/api/v1"
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
