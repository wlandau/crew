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
    worker_launch = function(worker) {
      plan_old <- future::plan()
      on.exit(future::plan(plan_old, .cleanup = FALSE))
      future::plan(private$plan, .cleanup = FALSE)
      expr <- quote(
        crew::crew_worker(
          worker = worker,
          store = store,
          jobs = jobs,
          timeout = timeout,
          wait = wait
        )
      )
      globals <- list(
        worker = worker,
        store = private$store$marshal(),
        jobs = private$jobs,
        timeout = private$timeout,
        wait = private$wait
      )
      handle <- future::future(
        expr = expr,
        substitute = FALSE,
        packages = "crew",
        globals = globals,
        lazy = FALSE,
        seed = TRUE
      )
      crew_wait(
        fun = function(handle) !future::resolved(handle),
        args = list(handle = handle),
        timeout = private$timeout,
        wait = private$wait
      )
      handle
    },
    worker_up = function(handle) {
      !is.null(handle) && !future::resolved(handle)
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
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
      private$plan <- plan
      invisible()
    },
    get_plan = function() {
      private$plan
    }
  )
)
