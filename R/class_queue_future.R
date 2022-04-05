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
    worker_run = function(handle, worker, fun, args) {
      plan_old <- future::plan()
      on.exit(future::plan(plan_old, .cleanup = FALSE))
      future::plan(private$plan, .cleanup = FALSE)
      task <- list(fun = deparse(fun), args = args)
      private$store$write_worker_input(worker = worker, value = task)
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
          store = private$store$marshal(),
          timeout = private$timeout,
          wait = private$wait
        ),
        lazy = FALSE,
        seed = TRUE
      )
    },
    update_all = function() {
      private$update_work()
    },
    worker_reuse = function(handle) {
      NULL
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
