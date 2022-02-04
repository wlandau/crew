#' @title Local `future` worker class.
#' @export
#' @aliases worker_future_local
#' @description `R6` class for a local `future` worker.
#'   It is "local" in the sense that the data store
#'   is a local file system. Workers can use `future.batchtools`.
class_worker_future_local <- R6::R6Class(
  classname = "worker_future_local",
  inherit = class_worker_future,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @description Launch the worker.
    launch = function() {
      if (self$up()) {
        return()
      }
      expr <- substitute(
        crew::crew_loop_worker_local(
          name = name,
          dir_root = dir_root,
          timeout = timeout,
          wait_input = wait_input
        ),
        env = list(
          name = self$name,
          dir_root = self$crew$store$dir_root,
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
    }
  )
)
