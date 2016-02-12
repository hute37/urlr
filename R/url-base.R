#' "Virtual" object initialization
#'
#' @rdname URL_base
#' @param x an URL object
#' @param ... further arguments
#' @return an list of URL obects
#' @export
#'
URL_init <- function(x, ...)
  UseMethod("URL_init")

#' @export
URL_init.default <- function(x, ...) {
  result <- x
  result
}

#' macro substitution
#'
#' @rdname URL_base
#' @return an list of URL obects
#' @export
#'
URL_config <- function(x, ...)
  UseMethod("URL_config")

#' @export
URL_config.default <- function(x, ...) {
  warning(x)
  print(paste("URL_config",format(x)))
  result <- list(x)
  result
}

#' macro substitution
#'
#' @rdname URL_base
#' @return an list of URL obects
#' @export
#'
URL_expand <- function(x, ...)
  UseMethod("URL_expand")

#' @export
URL_expand.default <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}

#' params
#'
#' @rdname URL_base
#' @return an list of URL obects
#' @export
#'
URL_fetch <- function(x, ...)
  UseMethod("URL_fetch")

#' @export
URL_fetch.default <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}


#' params
#'
#' @rdname URL_base
#' @return an list of URL obects
#' @export
#'
URL_parse <- function(x, ...)
  UseMethod("URL_parse")

#' @export
URL_parse.default <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}



#' params
#'
#' @rdname URL_base
#' @return an list of URL obects
#' @export
#'
URL_collect <- function(x, ...)
  UseMethod("URL_collect")

#' @export
URL_collect.default <- function(x, ...) {
  warning(x)
  result <- list(x)
  result
}


