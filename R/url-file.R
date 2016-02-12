#
#' @export
URL_init.URL_file <- function(x, ...) {
  result <- x
  result
}

#' @export
URL_config.URL_file <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

#' @export
URL_fetch.URL_file <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

#' @export
URL_parse.URL_file <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

#' @export
URL_collect.URL_file <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

