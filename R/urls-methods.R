#' @include URLS.R

.to_urls <- function(xs = NULL) {
  if (is.null(xs) || length(xs) == 0) {
    return(list())
  }
  if (is.URL(xs)) {
    return(list(xs))
  }
  if (!is.list(xs)) {
    xs <- as.list(xs)
  }
  xs <- purrr::map(xs,.to_urls)
  result <- purrr::flatten(xs)
  result
}

.map_urls <- function(xs, f, ...) {
  xs <- .to_urls(xs)
  stopifnot(all(sapply(xs,is.URL)))
  us <- purrr::map(xs, function(x) f(x, ...))
  result <- .to_urls(us)
  result
}


#' create a list of URL fron scala or list(vector arguments
#'
#' Invoke URL constructor, initialize urls and the wrao in URLS object
#' @rdname us
#' @param x a scalar/list parameter to URL constructor
#' @param ... further arguments
#' @return A URLS object
#' @export
us <- function(x, ...)
  standardGeneric("us")

#' @rdname us
#' @export
setGeneric("us")

#' @rdname us
setMethod("us", signature(x = "URLS"), function(x, ...) {
  x
})

#' @rdname us
setMethod("us", signature(x = "ANY"), function(x, ...) {
  us <- URL(x, ...)
  us <- .to_urls(us)
  us <- .map_urls(us,URL_init, ...)
  result <- URLS(urls=us)
  result
})


