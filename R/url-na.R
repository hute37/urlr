#
#' @export
URL_init.URL_na <- function(x, ...) {
  result <- x
  result
}

#' @export
URL_config.URL_na <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

#' @export
URL_fetch.URL_na <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

#' @export
URL_parse.URL_na <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

#' @export
URL_collect.URL_na <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

