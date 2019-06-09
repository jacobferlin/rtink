#' Retrieve Accounts
#'
#' Retrieve accounts from authenticated login used in token.
#'
#' @param token Required. Created by \code{token("accounts")}.
#'
#' @return A tbl.
#'
#' @examples
#' tok <- token("accounts")
#' tbl <- accounts(tok)
#'
#' @export
accounts <- function(token) {
  parsed <- get_accounts(token)$parsed
  li     <- parsed[["accounts"]]
  to_tibble(li)
}

#' Retrieve Transactions
#'
#' Retrieve transactions from authenticated login used in token.
#'
#' @param token Required. Created by \code{token("transactions")}.
#'
#' @return A tbl.
#'
#' @examples
#' tok <- token("transactions")
#' tbl <- transactions(tok)
#'
#' @export
transactions <- function(token) {
  parsed <- get_transactions(token)$parsed
  li     <- parsed
  to_tibble(li)
}

#' Retrieve Investments
#'
#' Retrieve investments from authenticated login used in token.
#'
#' @param token Required. Created by \code{token("investments")}.
#'
#' @return A tbl.
#'
#' @examples
#' tok <- token("investments")
#' tbl <- investments(tok)
#'
#' @export
investments <- function(token) {
  parsed <- get_investments(token)$parsed
  li     <- parsed[["portfolios"]]
  to_tibble(li)
}

#' Retrieve Instruments
#'
#' Retrieve instruments from authenticated login used in token.
#'
#' @param token Required. Created by \code{token("instruments")}.
#'
#' @return A tbl.
#'
#' @examples
#' tok <- token("instruments")
#' tbl <- instruments(tok)
#'
#' @export
instruments <- function(token) {
  parsed      <- get_investments(token)$parsed
  portfolios  <- parsed[["portfolios"]]
  instruments <- purrr::map(portfolios, "instruments")
  tbl_list    <- lapply(instruments, to_tibble)
  dplyr::bind_cols(tbl_list)
}

to_tibble <- function(li) {

  # Turn list of lists (li) into single tibble
  tbl <- purrr::transpose(li) %>%
    purrr::map(flatten_dynamic) %>%
    tibble::as_tibble()

  # Convert UNIX Epoch Time in milliseconds to datetime
  is_unix_epoch_ms <- function(col) is.numeric(col) && all(col > 1e+12)
  as_datetime_ms   <- function(ms) lubridate::as_datetime(ms * 1e-3)
  tbl <- dplyr::mutate_if(tbl, is_unix_epoch_ms, as_datetime_ms)

  tbl
}

flatten_dynamic <- function(col) {

  first_item <- col[[1]]

  if (is.character(first_item)) return(purrr::flatten_chr(col))
  if (is.integer(first_item))   return(purrr::flatten_int(col))
  if (is.numeric(first_item))   return(purrr::flatten_dbl(col))
  if (is.logical(first_item))   return(purrr::flatten_lgl(col))

  col
}
