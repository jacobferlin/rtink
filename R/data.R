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
      content = parsed,
      path = path,
      response = resp
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
