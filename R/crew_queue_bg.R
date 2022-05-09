# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
crew_queue_bg <- R6::R6Class(
  classname = "crew_queue_bg",
  inherit = crew_queue,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    initialize_workers = function(workers) {
      super$initialize_workers(workers)
      private$workers$handle <- map(
        private$workers$worker,
        ~callr::r_bg(
          func = crew_queue_worker_start,
          args = list(
            worker = .x,
            store = private$store$marshal(),
            max_tasks = private$max_tasks,
            timeout = private$timeout,
            wait = private$wait
          ),
          supervise = TRUE
        )
      )
      crew_wait(
        fun = ~all(map_lgl(private$workers$handle, ~.x$is_alive())),
        timeout = private$timeout,
        wait = private$wait,
        message = "timed out waiting for bg workers to start."
      )
    },
    worker_start = function(worker) {
      crew_error(sprintf("bg worker %s should already be up.", worker))
    },
    worker_up = function(handle, worker = NULL) {
      length(handle) && handle$is_alive()
    },
    update_crashed = function() {
      crashed <- !map_lgl(private$workers$worker, private$worker_up_log)
      if (any(crashed)) {
        workers <- paste(private$workers$worker[crashed], collapse = ", ")
        crew_error(paste("crashed bg workers:", workers))
      }
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      wait = 0.1,
      store = crew_store_local$new(timeout = Inf, wait = wait)
    ) {
      super$initialize(
        workers = workers,
        store = store,
        timeout = Inf,
        wait = wait,
        max_tasks = Inf
      )
    },
    shutdown = function() {
      super$shutdown()
      for (handle in private$workers$handle) {
        if (length(handle)) {
          handle$kill()
        }
      }
      invisible()
    }
  )
)
