#' @title `callr` worker class.
#' @export
#' @aliases worker_callr
#' @description `R6` class for a `callr` worker.
class_worker_callr <- R6::R6Class(
  classname = "worker_callr",
  inherit = class_worker,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field process A `callr::r_bg()` process handler.
    process = NULL,
    #' @description Launch the worker.
    launch = function() {
      if (self$up()) {
        return()
      }
      self$process <- callr::r_bg(
        func = crew::crew_worker_loop,
        args = list(
          name = self$name,
          store = self$crew$store$marshal(),
          timeout = self$timeout,
          wait_input = self$wait_input
        ),
        supervise = TRUE
      )
      invisible()
    },
    #' @description `TRUE` if the worker is up and `FALSE` otherwise.
    up = function() {
      if_any(
        is.null(self$process),
        FALSE,
        self$process$is_alive()
      )
    },
    #' @description Gracefully shut down the worker.
    shutdown = function() {
      if (!is.null(self$process)) {
        self$process$kill()
      }
      invisible()
    },
    #' @description Worker validator.
    validate = function() {
      super$validate()
      crew_assert(is.null(self$process) || inherits(self$process, "r_process"))
    }
  )
)
