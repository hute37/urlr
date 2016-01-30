


#' error handler for URL functions
#'
#' @rdname URL_stop
#' @param x a URL object
#' @param caller a caller method name
#' @param text error message
#' @param ... other arguments passed to specific methods
#' @return raises a stop condition
#' @export
#'
URL_stop <- function(x, caller, text, ...)
  UseMethod("URL_stop")

#' @rdname URL_stop
#' @method URL_stop default
#' @export
#'
URL_stop.default <- function(x, caller, text, ...) {
  m <- paste(c("method:",caller,text,"URL:",x$url))
  stop(m)
}


#' unsupported operation error
#'
#' @rdname URL_stop_unsupported
#' @param x a URL object
#' @param caller a caller method name
#' @param ... other arguments passed to specific methods
#' @return raises a stop condition
#' @export
#'
URL_stop_unsupported <-
  function(x, caller, ...)
    UseMethod("URL_stop_unsupported")

#' @rdname URL_stop_unsupported
#' @export
#' @method URL_stop_unsupported default
#'
URL_stop_unsupported.default <- function(x, caller, ...) {
  URL_stop(x,caller = caller, text = "unsupported operation", ...)
}


