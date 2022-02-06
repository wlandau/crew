#' @title Future worker class.
#' @export
#' @aliases worker_future
#' @description `R6` class for a `future` worker.
#' @examples
#' if (requireNamespace("future.callr", quietly = TRUE)) {
#' future::plan(future.callr::callr)
#' crew <- class_crew$new(worker_classes = list(class_worker_future))
#' crew$recruit(workers = 1)
#' worker <- crew$workers[[1]]
#' worker$send(fun = function(arg) paste("job", arg), args = list(arg = 1))
#' while (!worker$receivable()) Sys.sleep(0.1)
#' job <- worker$receive()
#' print(job$value)
#' print(job$error)
#' worker$shutdown()
#' future::plan(future::sequential)
#' }
class_worker_future <- R6::R6Class(
  classname = "worker_future",
  inherit = class_worker,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field future A `future::future()` object.
    future = NULL,
    #' @description Launch the worker.
    #' @return `NULL` (invisibly).
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
          store = self$crew$store$marshal(),
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
    #' @details While running, the worker could be actually running a job,
    #'   or it could be waiting for job input. This way, a worker
    #'   can accept multiple jobs throughout its lifetime before it
    #'   times out and is possibly relaunched. This is what it means
    #'   to be a "semi-persistent" worker.
    #' @return `TRUE` if the worker is running and `FALSE` otherwise.
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
