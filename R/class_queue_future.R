# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
queue_future <- R6::R6Class(
  classname = "queue_future",
  inherit = queue,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    plan = NULL,
    processes = NULL,
    worker_run = function(handle, worker, fun, args) {
      task <- list(fun = deparse(fun), args = args)
      private$store$write_worker_input(worker = worker, value = task)
      private$process_wait()
      process <- callr::r_bg(
        func = function(worker, store, timeout, wait, plan) {
          # executed inside the worker
          # nocov start
          plan_old <- future::plan()
          on.exit(future::plan(plan_old, .cleanup = FALSE))
          future::plan(plan, .cleanup = FALSE)
          future::future(
            expr = crew::crew_job(
              worker = worker,
              store = store,
              timeout = timeout,
              wait = wait
            ),
            substitute = TRUE,
            packages = character(0),
            globals = list(
              worker = worker,
              store = store,
              timeout = timeout,
              wait = wait
            ),
            lazy = FALSE,
            seed = TRUE
          )
          # nocov end
        },
        args = list(
          worker = worker,
          store = private$store$marshal(),
          timeout = private$timeout,
          wait = private$wait,
          plan = private$plan
        ),
        supervise = TRUE
      )
      handle <- new.env(parent = emptyenv())
      handle$process <- process
      handle
    },
    worker_up = function(handle) {
      process <- handle$process
      if (is.null(process)) {
        return(FALSE)
      }
      if (process$is_alive()) {
        return(TRUE)
      }
      future <- crew_catch_crash(process$get_result())
      if (all(is.logical(future))) {
        resolved <- !any(future)
        if (resolved) {
          handle$process <- NULL
        }
        return(resolved)
      }
      private$process_wait()
      handle$process <- callr::r_bg(
        func = function(future) future::resolved(future), # nocov
        args = list(future = future)
      )
      TRUE
    },
    process_up = function(handle) {
      if_any(is.null(handle$process), FALSE, handle$process$is_alive())
    },
    process_available = function() {
      up <- map_lgl(private$workers$handle, private$process_up)
      sum(up) < private$processes
    },
    process_wait = function() {
      crew_wait(
        private$process_available,
        timeout = private$timeout,
        wait = private$wait,
        message = "timed out waiting for local processes to be available."
      )
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      processes = 1,
      jobs = Inf,
      timeout = 60,
      wait = 0.1,
      plan = future::plan()
    ) {
      super$initialize(
        workers = workers,
        jobs = jobs,
        timeout = timeout,
        wait = wait
      )
      private$processes <- processes
      private$plan <- plan
      invisible()
    },
    get_plan = function() {
      private$plan
    }
  )
)
