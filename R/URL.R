#' Create a URL S3 object
#'
#' @rdname URL
#' @param url string url rapresentation
#' @param base a base url for relative urls (string or URL)
#' @param src an URL object fron which this object is derived
#' @param id entry id (taken from named list/array)
#' @param meta an optional array of meta attributes
#' @param ... other arguments passed to specific methods
#' @return an URL object
#' @export
#' @examples
#' # simple scalar construction
#' u <- URL('http://www.w3c.org')
#' str(u)
#' as.character(u)
#'
#' # "derived" URL
#' u2 <- URL('http://ietf.org', ,src='http://www.w3c.org')
#' as.character(u2)
#' as.character(u2$src)
#'
#' # default schema ('file://') with defaults
#'
#' s1 <- URL('/etc/R/Renvironment')
#' s2 <- URL('hosts',base='/etc')
#' s3 <- URL('.Rhistory')
#' s4 <- URL('~/.Rprofile')
#' s5 <- URL('.Rprofile', base='~')
#'
#' sapply(list(s1,s2,s3,s4,s5), as.character)
#'
#' # construction from a (named) character vector and data.frame cast
#' l <- c(
#'     w3c='http://www.w3c.org',
#'     ietf='http://ietf.org',
#'     iana='http://www.iana.org',
#'     hosts='hosts',
#'     'R env'='/etc/R/Renviron'
#'  )
#'
#'  uz <- URL(l, src=URL('https://cran.r-project.org/'), base='/etc')
#'
#'  df <- as.data.frame.URL(uz)
#'
#'  head(df,nrow(df))
#'
URL <- function(url, base = paste0("file://",getwd()), src = NULL, id = NULL, meta = c(), ...) {
  if (missing(url) || is.null(url))
    return(NULL)
  if (is.URL(url)) {
    return(URL(url$url,src=ifelse(is.null(src), url,src)))
  }
  if (is.list(url)) {
    return(mapply(function(u,n)
      URL(url=u, base=base, src=src, id=n, meta=meta, ...), u=url, n=names(url), SIMPLIFY = FALSE))
  }
  stopifnot(is.character(url))
  if (length(url) > 1) {
    return(mapply(function(u,n)
      URL(url=u, base=base, src=src, id=n, meta=meta, ...), u=url, n=names(url), SIMPLIFY = FALSE))
  }

  init <- NULL
  if (!is.null(src)) {
    if (!is.URL(src)) {
      stopifnot(is.character(src))
      src <- URL(src)
    }
  }
  url <- as.character(url)
  rel <- NULL
  schemeless <- !grepl('^[a-z]+:',url)
  relative <- schemeless && !grepl('^[/~]',url)
  if (schemeless) {
    if (relative) {
      if (!is.URL(base)) {
        base <- URL(base)
      }
      rel <- url
      url <- paste(base$url,rel, sep="/") # TODO(gp): url canon

    } else {
      base <- NULL
      url <- paste0("file://", path.expand(url))
    }

  } else {
    base <- NULL
  }


  comp <- urltools::url_parse(url)

  if (is.null(src)) {
    origin <- TRUE
    level <- 1
    init <- NULL
    src <- NULL
    env <- new.env()
  } else {
    origin <- FALSE
    level <- src$level + 1
    env <- new.env(parent = src$env)
    init <- src
    while (!is.null(init$src)) {
      init <- init$src
    }
  }

  if (is.null(id)) {
    id <- NA_character_
  }

  x <- structure(list(
    id = id,
    url = url,
    comp = comp,
    src = src,
    init = init,
    base = base,
    rel = rel,
    env = env,
    relative = relative,
    origin = origin,
    level = level
  ),class = c(paste("URL",comp$scheme,sep = "_"), "URL"))

  if (is.null(x$init)) {
    x$init <- x # self-ref
  }

  x <- URL_init(x, meta = meta, ...) # warning StacKOverflow
  x <- URL_meta(x, meta = meta)
}



#' check if URL object
#'
#' @rdname URL
#' @param x an arbitrary object or an object of S3 class URL
#' @return check condition
#' @export
is.URL <- function(x)
  inherits(x, "URL")



#' conver as.character() string from of object to URL object
#'
#' @rdname URL
#' @return an URL object
#' @export
#'
as.URL <- function(x, ...)
  UseMethod("as.URL")

#' @rdname URL
#' @export
#'
as.URL.default <- base::Vectorize(
  FUN = function(x, ...) {
    url <- as.character(x)
    x <- URL(url, ...)
  }
)


#' @rdname URL
#' @method as.character URL
#' @export
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
#' @rdname URL
#' @return an named list of data.frame columns
#' @export
#'
as.URL_row <- function(x, ...)
  UseMethod("as.URL_row")


#' @rdname URL
#' @method as.URL_row default
#' @export
#'
as.URL_row.default <- function(x, ...) {
  stopifnot(is.URL(x))
  as.NA.NULL <- function (o) ifelse(is.null(o), NA, o)
  unlist(list(
    id = as.NA.NULL(as.character(x$id)),
    url = x$url,
    base = as.NA.NULL(as.character(x$base)),
    src = as.NA.NULL(as.character(x$src)),
    init = as.NA.NULL(as.character(x$init)),
    relative = x$relative,
    origin = x$origin,
    level = x$level,
    scheme = x$comp$scheme,
    domain = x$comp$domain,
    port = x$comp$port,
    path = x$comp$path,
    parameter = x$comp$parameter,
    fragment = x$comp$fragment
  ))
}



#' coerce an URL vector to a data.frame
#'
#' @rdname URL
#' @param row.names same as as.data.frame
#' @param optional same as as.data.frame
#' @param RECURSIVE deep expand src URLs
#' @method as.data.frame URL
#' @export
as.data.frame.URL <- function(x, row.names = NULL, optional = FALSE, RECURSIVE=FALSE, ...) {
  if (missing(x) || is.null(x))
    return(as.data.frame(list()))
  if (!is.URL(x) && is.list(x)) {
    #@see http://www.r-bloggers.com/concatenating-a-list-of-data-frames/
    xs <- lapply(x, as.data.frame.URL)
    df <- do.call('rbind',xs)
    df <- as.data.frame(df, row.names = row.names, optional = optional, ...)
    return(df)
  }
  stopifnot(is.URL(x))
  df <- data.frame(t(as.matrix(as.URL_row(x, ...))))
  as.data.frame(df, row.names = row.names, optional = optional, ...)
}







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
