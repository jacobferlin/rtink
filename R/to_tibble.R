to_tibble <- function(li) {

  # Turn list of lists (li) into single tibble
  # df_list:
  # [[1]]
  #     accountNumber = 423904834
  #     availableCredit = 0
  #     ...
  # [[2]]
  #     accountNumber = 423904834
  #     availableCredit = 0
  #     ...
  tbl <- purrr::transpose(li) %>%
    purrr::map(flatten_dynamic) %>%
    tibble::as_tibble()

  # Convert UNIX Epoch Time in milliseconds to datetime
  is_unix_epoch_ms <- function(col) is.numeric(col) && all(col > 1e+12)
  as_datetime_ms   <- function(ms) lubridate::as_datetime(ms * 1e-3)
  tbl <- dplyr::mutate_if(tbl, is_unix_epoch_ms, as_datetime_ms)

  tbl
}

user <- function(token) {
  parsed <- get_user(token)$parsed
  li     <- list(parsed)
  to_tibble(li)
}

profile <- function(token) {
  parsed  <- get_user(token)$parsed
  profile <- parsed$profile
  li      <- list(profile)
  to_tibble(li)
}

#' @export
accounts <- function(token) {
  parsed <- get_accounts(token)$parsed
  li     <- parsed[["accounts"]]
  to_tibble(li)
}

#' @export
transactions <- function(token) {
  parsed <- get_transactions(token)$parsed
  li     <- parsed
  to_tibble(li)
}

#' @export
investments <- function(token) {
  parsed <- get_investments(token)$parsed
  li     <- parsed[["portfolios"]]
  to_tibble(li)
}

instruments <- function(token) {
  parsed      <- get_investments(token)$parsed
  portfolios  <- parsed[["portfolios"]]
  instruments <- purrr::map(portfolios, "instruments")
  tbl_list    <- lapply(instruments, to_tibble)
  dplyr::bind_cols(tbl_list)
}

flatten_dynamic <- function(col) {

  first_item <- col[[1]]

  if (is.character(first_item)) return(purrr::flatten_chr(col))
  if (is.integer(first_item))   return(purrr::flatten_int(col))
  if (is.numeric(first_item))   return(purrr::flatten_dbl(col))
  if (is.logical(first_item))   return(purrr::flatten_lgl(col))

  col
}
