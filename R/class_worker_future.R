#' @title Future worker class.
#' @export
#' @aliases worker_future
#' @description `R6` class for a `future` worker.
class_worker_future <- R6::R6Class(
  classname = "worker_future",
  inherit = class_worker,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field future A `future::future()` object.
    future = NULL,
    #' @description `TRUE` if the worker is alive and `FALSE` otherwise.
    alive = function() {
      !future::resolved(self$future)
    },
    #' @description Worker validator.
    validate = function() {
      super$validate()
      crew_assert(is.null(self$future) || inherits(self$future, "Future"))
    }
  )
)
