#' Create a URL S3 object
#'
#' @param url string url rapresentation
#' @param base a base url for relative urls (string or URL)
#' @param src an URL object fron which this object is derived
#' @param meta an optional array of meta attributes
#' @param ... other arguments passed to specific methods
#' @return an URL object
#' @export
#'
URL <- function(url, base = paste0("file://",getwd()), src = NULL, meta = c(), ...) {
  if (missing(url) || is.null(url))
    return(NULL)
  if (is.URL(url)) {
    return(URL(url$url,src=ifelse(is.null(src), url,src)))
  }
  if (is.list(url)) {
    return(lapply(url, function(u)
      URL(u, base, src, meta, ...)))
  }

  if (!is.null(src)) {
    if (!is.URL(src)) {
      stopifnot(is.character(src))
      src <- URL(src)
    }
  }
  url <- as.character(url)
  rel <- NULL
  schemeless <- !grepl('^[a-z]+:',url)
  relative <- schemeless && (substr(url,1,1) != "/")
  if (schemeless) {
    if (relative) {
      if (!is.URL(base)) {
        base <- URL(base)
      }
      rel <- url
      url <- paste(base$url,rel, sep="/") # TODO(gp): url canon

    } else {
      base <- NULL
      url <- paste0("file://", url)
    }

  } else {
    base <- NULL
  }


  comp <- urltools::url_parse(url)

  if (is.null(src)) {
    origin <- TRUE
    src <- NULL
    env <- new.env()
  } else {
    origin <- FALSE
    env <- new.env(parent = src$env)
  }

  x <- structure(list(
    url = url,
    comp = comp,
    src = src,
    base = base,
    rel = rel,
    env = env,
    relative = relative,
    origin = origin
  ),class = c(paste("URL",comp$scheme,sep = "_"), "URL"))


  x <- URL_init(x, meta = meta, ...) # warning StacKOverflow
  x <- URL_meta(x, meta = meta)
}

#' check if URL object
#'
#' @param x an object to check
#' @return check condition
#' @export
is.URL <- function(x)
  inherits(x, "URL")


#' @export
#' @method as.character URL
as.character.URL <- function(x, ...) {
  if (missing(x) || is.null(x))
    return(NULL)
  if (is.URL(x))
    return(x$url)
  if (is.list(x))
    return(lapply(x, as.character.URL))
  stopifnot(is.URL(x))
}

#' conver string form of object to URL object
#'
#' @rdname as.URL
#' @param x somthing to be cast to an URL
#' @param ... other arguments passed to specific methods
#' @return an URL object
#' @export
#'
as.URL <- function(x, ...)
  UseMethod("as.URL")

#' @rdname as.URL
#' @export
#' @method as.URL default
#'
as.URL.default <- base::Vectorize(
  FUN = function(x, ...) {
    url <- as.character(x)
    x <- URL(url, ...)
  }
)





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
#' @export
#' @method URL_stop default
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
#' @param ... other arguments passed to specific methods
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
