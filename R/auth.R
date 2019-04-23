token <- function(scope = c("accounts", "transactions"),
                  force_new = FALSE,
                  market = "SE") {

  if (scope == "all") scope <- allowed_scopes()

  # Assert scope
  not_allowed_pos <- which(!(scope %in% allowed_scopes()))
  assertthat::assert_that(
    all(scope %in% allowed_scopes()),
    msg = paste0(
      "Not allowed scope(s): ",
      paste0(scope[not_allowed_pos], collapse = ", "))
  )

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
    query_authorize_extra = list(market = market)
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

has_scope <- function(token, scope) {

  # Assert scope
  not_allowed_pos <- which(!(scope %in% allowed_scopes()))
  assertthat::assert_that(
    all(scope %in% allowed_scopes()),
    msg = paste0(
      "Scope(s) not available in API: ",
      paste0(scope[not_allowed_pos], collapse = ", "))
  )

  token_scope_str  <- token$credentials$scope
  token_scope_list <- stringr::str_extract_all(token_scope_str, "[a-zA-Z]+(?=:read)")
  token_scope_vec  <- token_scope_list[[1]]

  scope %in% token_scope_vec
}

allowed_scopes <- function() {
  c("user",
    "credentials",
    "accounts",
    "transactions",
    "investments",
    "statistics")
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
