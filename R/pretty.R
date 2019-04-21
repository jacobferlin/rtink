pretty <- function(l, cols) {

  # All cols?
  if (length(cols) == 1 && cols == "all") cols <- names(l[[1]])

  # Helper function to extract all named col from list
  # and put into a tibble with a single column
  create_tibble_col <- function(col_name) {
    tibble::tibble(!!col_name := purrr::map(l, col_name))
  }

  # Extract all columns, bind them into one tibble,
  # and flatten into correct type/class
  df_list <- lapply(cols, function(x) create_tibble_col(x))
  df      <- dplyr::bind_cols(df_list)
  df      <- purrr::map(df, flatten_dynamic)
  df      <- tibble::as_tibble(df)

  # Convert UNIX Epoch Time in milliseconds to datetime
  is_unix_epoch_ms <- function(col) is.numeric(col) && all(col > 1e+12)
  as_datetime_ms   <- function(ms) lubridate::as_datetime(ms * 1e-3)
  df <- dplyr::mutate_if(df,
                         is_unix_epoch_ms,
                         dplyr::funs(as_datetime_ms))

  df
}

pretty_accounts <- function(parsed_content,
                            cols = c("id", "accountNumber", "name", "balance")) {
  l <- parsed_content[["accounts"]]
  pretty(l, cols = cols)
}

pretty_transactions <- function(parsed_content,
                                cols = c("id", "description", "amount", "date")) {
  l <- parsed_content
  pretty(l, cols = cols)
}

pretty_investments <- function(parsed_content,
                               cols = "all") {
  l <- parsed_content[["portfolios"]]
  pretty(l, cols = cols)
}

pretty_instruments <- function(parsed_content,
                               cols = "all") {
  portfolios  <- parsed_content[["portfolios"]]
  instruments <- purrr::map(portfolios, "instruments")
  df_list     <- lapply(instruments, function(x) pretty(x, cols = cols))
  dplyr::bind_cols(df_list)
}

flatten_dynamic <- function(col) {

  first_item <- col[[1]]

  if (is.character(first_item)) return(purrr::flatten_chr(col))
  if (is.integer(first_item))   return(purrr::flatten_int(col))
  if (is.numeric(first_item))   return(purrr::flatten_dbl(col))
  if (is.logical(first_item))   return(purrr::flatten_lgl(col))

  col
}
