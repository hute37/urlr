#' Assiggn S3 classes to urls
#'
#' @rdname URL_class
#' @param url string url rapresentation
#' @param comp a parsed url
#' @param src source url
#' @param id name of url instance (in dotted form)
#' @param classes extra classes to include (between schema and id)
#' @param ... other arguments passed to specific methods
#' @return an array of string to be assigned as S3 classes
#' @export
#'
URL_class <- function(url, comp = urltools::url_parse(url), src = NULL, id = NULL, classes=NULL, ...) {
  if (missing(url) || is.null(url))
    return(NULL)
  stopifnot(is.character(url))
  if (is.null(id)) {
    if (!is.null(src)) {
      id <- src$id
    }
  }
  if (is.null(id)) {
    id <- ''
  }
  idp <- strsplit(gsub(' ', '_',id), ".", fixed = TRUE)[[1]]
  clazzes <- list(paste("URL",comp$scheme,sep = "_"), "URL")
  if (!is.null(classes)) {
    clazzes <- append(clazzes,classes, after = 0)
  }
  for (i in seq_along(idp)) {
    clazz <- paste( "URL", paste(c(idp[1:i]), collapse = '_'), sep = '_')
    clazzes <- append(clazzes,clazz, after = 0)
  }
  result <- unlist(clazzes)
  result
}





#' Create a URL S3 object
#'
#' @rdname URL
#' @param url string url rapresentation
#' @param base a base url for relative urls (string or URL)
#' @param src an URL object fron which this object is derived
#' @param id entry id (taken from named list/array)
#' @param meta an optional array of meta attributes
#' @param classes extra classes to include (between schema and id)
#' @param ... other arguments passed to specific methods
#' @return an URL object
#' @export
#' @examples
#' # simple scalar construction
#' u <- URL('http://www.w3c.org')
#' as.character(u)
#' print(u)
#'
#' \dontrun{
#' str(u)
#' }
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
#'  df <- as.data.frame(as.URL_list(uz))
#'
#'  head(df,nrow(df))
#'
URL <- function(url, base = paste0("file://",getwd()), src = NULL, id = NULL, meta = c(), classes=NULL, ...) {
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
  ),class = URL_class(url = url, comp = comp, src = src, id = id, classes = classes, ... ))

  if (is.null(x$init)) {
    x$init <- x # self-ref
  }

  #x <- URL_init(x, meta = meta, ...) # warning StacKOverflow
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
as.URL.default <- function(x, ...) {
  if (missing(x) || is.null(x))  return(NULL)
  if (is.URL(x)) return(x)
  if (length(x) > 1) {
    return(mapply(function(u,n) {
        if (is.URL(u)) {
          u$id <- n
          return(u)
        } else {
          return(URL(url = u, id = n, ...))
        }
    },u = url, n = names(url), SIMPLIFY = FALSE
    ))
    return(x)
  }
  url <- as.character(x)
  x <- URL(url, ...)
}

#' @rdname URL
#' @export
#'
as.URL.list <- function(x, ...) {
  if (all(lapply(x,is.URL))) {
    class(x) <- append(class(x),'URL',after = 0)
    return(x)
  }
}






#' check if URL_list object
#'
#' @rdname URL_list
#' @param x an arbitrary object or an object of S3 class URL_list
#' @return check condition
#' @export
is.URL_list <- function(x)
  inherits(x, "URL_list")


#' add URL_list class to a generic list containing only URL objects.
#'
#' throw a conversion error if the list contains other stuff.
#'
#' @rdname URL_list
#' @param ... further arguments
#' @return an URL_list object
#' @export
#'
as.URL_list <- function(x, ...) UseMethod('as.URL_list')

#' @rdname URL_list
#' @export
as.URL_list.list <- function(x, ...) {
  if (is.URL_list(x)) return(x)
  stopifnot(all(sapply(x,is.URL)))
  class(x) <- append(class(x),'URL_list',after=0)
  x
}




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


#' @method format URL
#' @export
format.URL <- function(x,...) {
  return(paste0('[URL:',x$id,'](',x$url,')'))
}



#' @method print URL
#' @export
print.URL <- function(x, ...) {
  cat(paste0(sprintf("[%s]",x$id),
             '\t',as.character(x$url),
             '\t<- ',as.character(x$src),
             '\t<<- ',as.character(x$init),
             '\t',sprintf("<%s>", paste(class(x),collapse=',')),
             '\n'))
}

#' @method summary URL
#' @export
summary.URL <- function(object, ...) {
  x <- object
  cat(paste0(format(x),'\n'))
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



#' coerce an URL to a data.frame
#'
#' @rdname as.data.frame.URL
#' @param x any object
#' @param row.names same as as.data.frame
#' @param optional same as as.data.frame
#' @param RECURSIVE deep expand src URLs
#' @param ... further arguments
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

#' coerce an URL_list to a data.frame
#'
#' see http://www.r-bloggers.com/concatenating-a-list-of-data-frames/
#'
#' @rdname as.data.frame.URL
#' @method as.data.frame URL_list
#' @export
as.data.frame.URL_list <- function(x, row.names = NULL, optional = FALSE, RECURSIVE=FALSE, ...) {
  if (missing(x) || is.null(x))
    return(as.data.frame(list()))
  stopifnot(is.URL_list(x))
  xs <- lapply(x, as.data.frame.URL)
  df <- do.call('rbind',xs)
  df <- as.data.frame(df, row.names = row.names, optional = optional, ...)
  return(df)
}



