token <- function(new_token = FALSE,
                  cache_file = token_cache_filename()) {

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
  if (!token_cache_exist(filename)) {
    stop(
      paste0("No token cache with filename '", filename, "' exists."),
      call. = FALSE
    )
  }
  readRDS(filename)[[1]]
}

token_cache_exist <- function(filename) {

  # Check file exist
  file_exist <- file.exists(filename)
  if (!file_exist) return(FALSE)

  # If file exist, also check class
  token <- tryCatch(
    readRDS(filename)[[1]],
    error = function(cond) {
      return(FALSE)
    }
  )

  inherits(token, "Token2.0")
}

token_cache_filename <- function() {
  ".httr-oauth"
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
