#' Create Authentication Token
#'
#' @param scope Required character vector. Possible scopes are \code{"accounts",
#'   "transactions", "investments", "instruments"}. Multiple scopes can be used
#'   in same token.
#' @param force_new Required logical. Should new token be requested even though
#'   cache exist? If there has been a while since token cache was created, then
#'   this probably needs to be \code{TRUE}.
#' @param market Required string. Market (country) in which bank is accessed.
#' @param provider Optional string. Unique name of provider (eg.
#'   \code{sbab-bankid}). If this is provided, the provider will be chosen for
#'   the user. If not, the user can select a provider in a list.
#' @username Optional string. If this is provided, the username field will be
#'   pre-filled. If not, the user can type it in.
#'
#' @return An httr::oauth2.0_token object.
#'
#' @examples
#' token("accounts")
#' token(c("accounts", "transactions"))
#' token("accounts", force_new = TRUE)
#'
#' @export
token <- function(scope = c("accounts", "transactions"),
                  force_new = FALSE,
                  market = "SE",
                  provider = NULL,
                  username = NULL) {

  if (length(scope) == 1 && scope == "all") scope <- allowed_scopes()

  # Assert scope
  not_allowed_pos <- which(!(scope %in% allowed_scopes()))
  assertthat::assert_that(
    all(scope %in% allowed_scopes()),
    msg = paste0(
      "Not allowed scope(s): ",
      paste0(scope[not_allowed_pos], collapse = ", "))
  )

  if (force_new) {
    message("Disabling .tinkr-oauth by renaming to .tinkr-oauth-SUSPENDED")
    file.rename(".tinkr-oauth", ".tinkr-oauth-SUSPENDED")
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

  # Build extras
  extras <- list()
  if (!is.null(market))   extras %<>% append(list(market         = market))
  if (!is.null(provider)) extras %<>% append(list(input_provider = provider))
  if (!is.null(username)) extras %<>% append(list(input_username = username))

  # Retrive token
  token <- httr::oauth2.0_token(
    endpoint = endpoint,
    app = app,
    scope = scope,
    use_oob = TRUE,
    oob_value = "http://localhost:3000/callback",
    cache = ".tinkr-oauth",
    query_authorize_extra = extras
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

#' Does Token have Scope
#'
#' Checks if token includes a particular scope or not.
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
  c("accounts",
    "transactions",
    "investments",
    "instruments")
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
