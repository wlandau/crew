# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
queue_session <- R6::R6Class(
  classname = "queue_session",
  inherit = queue,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    initialize_workers = function(workers) {
      super$initialize_workers(workers)
      private$workers$handle <- replicate(
        workers,
        callr::r_session$new(
          wait = FALSE,
          options = callr::r_session_options(extra = list(supervise = TRUE))
        )
      )
      crew_wait(
        fun = ~all(
          map_lgl(private$workers$handle, ~callr_session_ready(.x))
        ),
        timeout = private$timeout,
        wait = private$wait,
        message = "timed out waiting for session workers to start."
      )
    },
    worker_run = function(handle, worker, task, input) {
      crew_catch_crash(handle$call(func = input$fun, args = input$args))
      handle
    },
    worker_up = function(handle, worker = NULL) {
      length(handle) && handle$is_alive()
    },
    update_done = function() {
      index <- which(private$workers$sent)
      handles <- private$workers$handle[index]
      crew_catch_crash({
        connections <- map(handles, ~.x$get_poll_connection())
        poll <- as.character(processx::poll(processes = connections, ms = 0))
      })
      private$workers$done[index] <- poll == "ready"
    },
    update_results = function() {
      for (index in seq_len(nrow(private$workers))) {
        if (private$workers$done[index]) {
          worker <- private$workers$worker[index]
          task <- private$workers$task[index]
          result <- private$workers$handle[[index]]$read()
          private$add_result(task = task, result = result)
          private$worker_free(worker)
        }
      }
    },
    update_crashed = function() {
      up <- map_lgl(private$workers$worker, private$worker_up_log)
      if (!all(up)) {
        workers <- private$workers$worker[!up]
        crew_error(
          paste("crashed session workers:", paste(workers, collapse = ", "))
        )
      }
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      timeout = 60,
      wait = 0.1
    ) {
      private$timeout <- timeout
      private$wait <- wait
      private$initialize_tasks()
      private$initialize_results()
      private$initialize_workers(workers = workers)
      invisible()
    },
    push = function(
      fun,
      args = list(),
      task = crew_name(),
      update = TRUE
    ) {
      super$push(fun = fun, args = args, task = task, update = update)
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
