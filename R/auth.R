token <- function(scope = c("accounts", "transactions"),
                  force_new = FALSE) {

  if (force_new) {
    message("Disabling .httr-oauth by renaming to .httr-oauth-SUSPENDED")
    file.rename(".httr-oauth", ".httr-oauth-SUSPENDED")
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

  # Build scope
  scope <- paste0(scope, ":read", collapse = ",")

  # Retrive token
  token <- httr::oauth2.0_token(
    endpoint = endpoint,
    app = app,
    scope = scope,
    use_oob = TRUE,
    oob_value = "http://localhost:3000/callback",
    cache = TRUE,
    query_authorize_extra = list(market = "SE")
  )

  # Check class
  if (!inherits(token, "Token2.0")) {
    stop(
      "Could not retrieve a valid token",
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
