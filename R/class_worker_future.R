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
    #' @description Launch the worker.
    launch = function() {
      if (self$up()) {
        return()
      }
      expr <- substitute(
        crew::crew_worker_loop(
          name = name,
          store = store,
          timeout = timeout,
          wait_input = wait_input
        ),
        env = list(
          name = self$name,
          store = self$crew$store,
          timeout = self$timeout,
          wait_input = self$wait_input
        )
      )
      self$future <- future::future(
        expr = expr,
        substitute = FALSE,
        globals = FALSE,
        packages = character(0)
      )
      invisible()
    },
    #' @description `TRUE` if the worker is running and `FALSE` otherwise.
    up = function() {
      !future::resolved(self$future)
    },
    #' @description Worker validator.
    validate = function() {
      super$validate()
      crew_assert(is.null(self$future) || inherits(self$future, "Future"))
    }
  )
)
