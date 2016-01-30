#' "Virtual" object initialization
#'
#' @rdname URL_init
#' @param x an uninitialized URL object
#' @param meta an optional array of meta attributes
#' @param ... other arguments passed to specific methods
#' @return an initialized URL obect
#' @export
#'
URL_init <- function(x, meta = c(), ...)
  UseMethod("URL_init")

#' @rdname URL_init
#' @export
#' @method URL_init default
#'
URL_init.default <- function(x, meta = c(), ...)
  x

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




#' Fetch url to local storage
#'
#' @rdname URL_fetch
#' @param x an uninitialized URL object
#' @param ... other arguments passed to specific methods
#' @return a local file:// URL obect or na:// URL in case of failure
#' @export
#'
URL_fetch <- function(x, ...)
  UseMethod("URL_fetch")

#' @rdname URL_fetch
#' @export
#' @method URL_fetch default
#'
URL_fetch.default <- function(x, ...)
  x

#' follows 'R data()' and httr euristics to build a data rapresentation of an URL
#' the object returned is data.frame or environment
#'
#' Additional parameters are passed to read.* function
#'
#' @rdname URL_parse
#' @param x a URL object
#' @param ... other arguments passed to specific methods
#' @return an list of URL with relerence to internal environment for data object
#' @export
#'
URL_parse <- function(x, ...)
  UseMethod("URL_parse")

#' @rdname URL_parse
#' @export
#' @method URL_parse default
#'
URL_parse.default <-
  function(x, ...)
    URL_stop_unsupported(x,'URL_parse')



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
