#' @title Local `future` worker class.
#' @export
#' @description `R6` class for a local `future` worker.
#'   It is "local" in the sense that the data store
#'   is a local file system. Workers can use `future.batchtools`.
class_worker_future_local <- R6::R6Class(
  classname = "worker_future",
  inherit = class_worker_future,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @description Launch the worker.
    launch = function() {
      self$future <- future::future(
        expr = crew::crew_worker_local(
          name = name,
          dir_root = dir_root,
          timeout = timeout,
          wait_input = wait_input
        ),
        globals = self$globals(),
        packages = character(0)
      )
    }
  )
)
