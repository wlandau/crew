#' @title Future worker class.
#' @export
#' @description `R6` class for a `future` worker.
class_worker_future <- R6::R6Class(
  classname = "worker_future",
  inherit = class_worker,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field future A `future::future()` object.
    future = NULL,
    #' @description Create the `globals` argument of `future::future()`.
    globals = function() {
      list(
        name = self$name,
        dir_root = self$crew$store$dir_root,
        timeout = self$timeout,
        wait = self$wait_input
      )
    },
    #' @description Send a job.
    #' @param fun Function to run in the job. Should be completely
    #'   self-contained in the body and arguments, without relying
    #'   on the closure or global variables in the environment.
    #' @param args Named list of arguments to `fun`.
    send = function(fun, args = list()) {
      data <- list(fun = deparse(fun), args = args)
      self$crew$store$write_input(worker_name = self$name, data = data)
    },
    #' @description `TRUE` if the worker is alive and `FALSE` otherwise.
    alive = function() {
      !future::resolved(self$future)
    },
    #' @description `TRUE` if a worker is done with a job and the
    #'   main process can receive the output of the job. `FALSE` otherwise.
    done = function() {
      store$exists_output(worker_name = self$name)
    },
    #' @description Collect the results of a job.
    receive = function() {
      out <- store$read_output(worker_name = self$name)
      store$delete_output(worker_name = self$name)
      out
    },
    #' @description Terminate the worker.
    terminate = function() {
      self$send(fun = function() rlang::abort(class = "crew_terminate"))
    },
    #' @description Worker validator.
    validate = function() {
      super$validate()
      crew_assert(is.null(self$future) || inherits(self$future, "future"))
    }
  )
)
