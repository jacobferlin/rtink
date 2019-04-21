pretty <- function(l, cols) {

  if (length(cols) == 1 && cols == "all") cols <- names(l[[1]])

  create_tibble_col <- function(col_name) {
    tibble::tibble(!!col_name := purrr::map(l, col_name))
  }

  df_list <- lapply(cols, function(x) create_tibble_col(x))
  df      <- dplyr::bind_cols(df_list)
  df      <- apply(df, 2, function(x) flatten_dynamic(x))
  df      <- tibble::as_tibble(df)

  df
}

pretty_accounts <- function(parsed_content,
                            cols = c("id", "name", "balance")) {
  l <- parsed_content[["accounts"]]
  pretty(l, cols = cols)
}

pretty_transactions <- function(parsed_content,
                                cols = c("id", "description", "amount")) {
  l <- parsed_content
  pretty(l, cols = cols)
}

flatten_dynamic <- function(col) {

  first_item <- col[[1]]

  if (is.character(first_item)) return(purrr::flatten_chr(col))
  if (is.integer(first_item))   return(purrr::flatten_int(col))
  if (is.numeric(first_item))   return(purrr::flatten_dbl(col))
  if (is.logical(first_item))   return(purrr::flatten_lgl(col))

  col
}
