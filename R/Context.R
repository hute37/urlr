#' Create a mutable contetx object.
#'
#' A context object wraps ena enviroment abd in passed trhu comuptation steps.
#' It is also trasiently assigned tu url object durincg call dispatch.
#' The final environment is the "relurt" of piped computation
#' Data files can be produced also as computation "side-effects"
#'
#' @keywords internal
#' @aliases Context-class
#' @param configf a \code{URL_Config} YAML based object
#' @export
context <- function(config) UseMethod("context")

#' @export
context.URLConfig <- function(config) {
  Context$new(config = config, url = NULL, v = new.env())
}

#' @export
Context <-
  methods::setRefClass(
    "Context",
    fields = c("config", "url", "v"),
    methods = list(
      show = function() {
        cat("<Context> ", v, "\n", sep = "")
      },

      enter = function(u) {
        url <- u
      },

      exit = function() {
        url <- NULL
      },

      get_var = function(x) {
        result <- get(x,envir = v)
      },

      set_var = function(x, value) {
        assign(x,value,envir = v)
      },

      publish = function(pos = -1, envir = as.environment(pos)) {
        lapply(ls(envir = v), function(x)
          assign(x,get(x,envir = v),envir = envir))
      }

    )
  )



.ctx_retrieve <- function(us) {

}

.ctx_retrieve <- function(us) {

}

.ctx_retrieve <- function(us) {

}

.ctx_enter <- function(ctx,us) {

}

.ctx_exit <- function(ctx,us) {
  ctx$enter(u)

}


