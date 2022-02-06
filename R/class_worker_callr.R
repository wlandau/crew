#' @title `callr` worker class.
#' @export
#' @aliases worker_callr
#' @description `R6` class for a `callr` worker.
#' @examples
#' crew <- class_crew$new(worker_classes = list(class_worker_callr))
#' crew$recruit(workers = 1)
#' worker <- crew$workers[[1]]
#' worker$send(fun = function(arg) paste("job", arg), args = list(arg = 1))
#' while (!worker$receivable()) Sys.sleep(0.1)
#' job <- worker$receive()
#' print(job$value)
#' print(job$error)
#' worker$shutdown()
class_worker_callr <- R6::R6Class(
  classname = "worker_callr",
  inherit = class_worker,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field process A `callr::r_bg()` process handler.
    process = NULL,
    #' @description Launch the worker.
    #' @return `NULL` (invisibly).
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
    #' @description Check if the underlying worker R process
    #'   is actually running.
    #' @details While running, the worker could be actually running a job,
    #'   or it could be waiting for job input. This way, a worker
    #'   can accept multiple jobs throughout its lifetime before it
    #'   times out and is possibly relaunched. This is what it means
    #'   to be a "semi-persistent" worker.
    #' @return `TRUE` if the worker is running and `FALSE` otherwise.
    up = function() {
      if_any(
        is.null(self$process),
        FALSE,
        self$process$is_alive()
      )
    },
    #' @description Gracefully shut down the worker.
    #' @details The underlying worker process should promptly
    #'   shut down if successful. A new `send()` or `launch()`
    #'   call will re-launch the worker.
    #'
    #'   The shutdown method
    #'   for a `callr` worker is quicker and more reliable
    #'   than the default method of sending a shutdown job
    #'   through the data store.
    #' @return `NULL` (invisibly).
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
