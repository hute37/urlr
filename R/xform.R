#' set meta named array argument
#' as URL object attributes (with prefix 'meta.')
#'
#' @rdname URL_meta
#' @param x an uninitialized URL object
#' @param meta an optional array of meta attributes
#' @param ... other arguments passed to specific methods
#' @return a 'meta' decorated URL obect
#' @export
#'
URL_meta <- function(x, meta = c(), ...)
  UseMethod("URL_meta")

#' @rdname URL_meta
#' @export
#' @method URL_meta default
#'
URL_meta.default <- function(x, meta = c(), ...)
  x



#' cast to a url() connection
#'
#' TODO(gp): all 'as.connection' alternatives are needed (as.file, ...)
#'
#' @rdname as.conn.url
#' @param x an URL object
#' @param open     see base::url()
#' @param blocking see base::url()
#' @param encoding see base::url()
#' @param method   see base::url()
#' @param ... other arguments passed to specific methods
#' @return a base::connection from url(x$url, ...)
#' @export
#'
as.conn.url <- function(x, open = "", blocking = TRUE,
                        encoding = getOption("encoding"), method, ...)
  UseMethod("as.conn.url")

#' @rdname as.conn.url
#' @export
#' @method as.conn.url default
#'
as.conn.url.default <- function(x, open = "", blocking = TRUE,
                                encoding = getOption("encoding"), method, ...) {
  URL_stop_unsupported(x,'as.conn.url')
}



#' extract a persed data object, stored after 'parse'
#' in object environment
#'
#'
#' @rdname URL_data
#' @param x a URL object
#' @param ... further arguments
#' @return an parsed data object
#' @export
#'
URL_data <- function(x, ...)
  UseMethod("URL_data")

#' @rdname URL_data
#' @export
#' @method URL_data default
#'
URL_data.default <-
  function(x, ...)
    URL_stop_unsupported(x,'URL_data')
