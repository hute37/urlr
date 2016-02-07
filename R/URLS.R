# URLS class
#
# deps

setOldClass("URL")


check_URLS <- function(object) {
  errors <- character()
  if (!is(object, "URLS")) {
    msg <- "Wrong Argument."
    errors <- c(errors, msg)
  }
  if (!all(sapply(object@urls,is.URL))) {
    msg <- "Wrong urls list."
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) {
    TRUE
  } else {
    errors
  }
}


#' URLS class
#'
#' URL Collection (list)
#' Store a List of URLs as a list slot
#'
#' URLS are used as promary parameter and return value from URL transormation methods.
#'
#'
#' @name URLS-class
#' @rdname URLS-class
#' @param ... further arguments
#' @exportClass URLS
URLS <- setClass("URLS",
                 slots = c(urls = "list"),
                 prototype = list(urls = list()),
                 validity = function(object) {
                   errors <- character()
                   if (!is(object, "URLS")) {
                     msg <- "Wrong Argument."
                     errors <- c(errors, msg)
                   }
                   if (!all(sapply(object@urls,is.URL))) {
                     msg <- "Wrong urls list."
                     errors <- c(errors, msg)
                   }
                   if (length(errors) == 0) {
                     TRUE
                   } else {
                     errors
                   }
                 })

#' @rdname URLS-class
#' @param object an Object
#' @export
setMethod("show", signature(object = "URLS"), function(object) {
  x <- object
  v <- x@urls
  cat("URLS: len(",length(v),")\n\n")
  for (i in seq_along(v)) {
    cat(sprintf("[%d] ",i))
    print(v[[i]])
  }
  cat("\n")
})

#' @rdname URLS-class
#' @export
format.URLS <- function(x,...) {
  v <- x@urls
  s <- '{ URLS= ['
  if (length(v) == 0)
    return(paste0(s,']}'))
  sc=''
  for (i in seq_along(v)) {
    s <- paste(s,sc, format(v[[i]]))
    sc <- ', '
  }
  paste0(s,']}')
}

#' @rdname URLS-class
#' @param x an Object
#' @export
is.URLS <- function(x)
  is(x, c("URLS"))



#' Change an object to a URLS (collection of ULRs)
#'
#' @rdname as.URLS
#' @param x Object to be coerced to a duration
#' @return A URLS object
#' @keywords classes manip methods chron
#' @examples
#' \dontrun{
#' str(us)
#' }
#' @export
as.URLS <- function(x)
  standardGeneric("as.URLS")

#' @rdname as.URLS
#' @export
setGeneric("as.URLS")

#' @rdname as.URLS
setMethod("as.URLS", signature(x = "URLS"), function(x) {
  x
})

#' @rdname as.URLS
setMethod("as.URLS", signature(x = "list"), function(x) {
  stopifnot(all(sapply(x,is.URL)))
  x <- URLS(urls = x)
  x
})

#' @rdname as.URLS
setMethod("as.URLS", signature(x = "ANY"), function(x) {
  if (is.URL(x)) {
    return(as.URLS(list(x)))
  }
  u <- as.URL(x)
  if (is.URL(u)) {
    return(as.URLS(list(u)))
  }
  stop("only (castable) URL objects or list of URL objects are supported")
})



setAs("URLS", "list", function(from, to) {
    return(from@urls)
})




#' @export
print.URLS <- function(x, ...) {
  show(x)
}

#' @export
summary.URLS <- function(object, ...) {
  list(len = length(object@urls))
}
