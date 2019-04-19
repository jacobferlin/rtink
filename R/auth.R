get_token <- function() {

  # Build address to authenticate to bank through Tink
  tink_auth_addr    <- vector(mode = "character", length = 5L)
  tink_auth_addr[1] <- "https://oauth.tink.com/0.4/authorize/?client_id="
  tink_auth_addr[2] <- client_id()
  tink_auth_addr[3] <- "&redirect_uri=http://localhost:3000/callback&market="
  tink_auth_addr[4] <- "SE"
  tink_auth_addr[5] <- "&scope=accounts%3Aread"
  tink_auth_addr    <- paste0(tink_auth_addr, collapse = "")

  # "Send" link for user to specify bank and login
  tink_auth_addr

  endpoint <- httr::oauth_endpoint(
    authorize = "https://oauth.tink.com/0.4/authorize/",
    access    = "https://api.tink.se/api/v1/oauth/token"
  )
  app <- httr::oauth_app(
    "tink",
    client_id(),
    client_secret()
  )
  token2 <- httr::oauth2.0_token(
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

  "Original"
  "https://oauth.tink.com/0.4/authorize/"
  "?client_id=032799766bbc4eab876cf1ab69aa7010"
  "&redirect_uri=http://localhost:3000/callback"
  "&market=SE"
  "&scope=accounts:read"

  "New"
  "https://oauth.tink.com/0.4/authorize"
  "?client_id=032799766bbc4eab876cf1ab69aa7010"
  "&scope=accounts%3Aread"
  "&redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob"
  "&response_type=code"
  "&market=SE"

  # Retrive code (how?)

  # Code
  "http://localhost:3000/callback?code=d549aefb888b45f981e3f603e27d68b0"
  "http://localhost:3000/callback?code=42be61bbad7745cda77942bc135d29c8"
  "http://localhost:3000/callback?code=dd874b4e64da4f508fe03c65c5e47340"
  "http://localhost:3000/callback?code=87e998c4cac441bf88612ec5a988c69a"
  "http://localhost:3000/callback?code=cf7e641b08534f79a0067c8e02a15e91"
  "973666c94c1049aca63e4ea169ff3e0b"

  # Exchange code for (1) access token and (2) refresh token
  body <- list(
    code          = "cf7e641b08534f79a0067c8e02a15e91",
    client_id     = client_id(),
    client_secret = client_secret(),
    grant_type    = "authorization_code"
  )
  r <- httr::POST(
    url = "https://api.tink.se/api/v1/oauth/token",
    body = body,
    encode = "form",
    httr::add_headers("Content-Type" = "application/x-www-form-urlencoded; charset=UTF-8"),
    httr::verbose()
  )

  # Assert Success
  assertthat::assert_that(
    httr::status_code(r) == 200,
    msg = paste("No success, status code", httr::status_code(r))
  )

  token <- httr::content(r)

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
