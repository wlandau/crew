# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
queue_callr <- R6::R6Class(
  classname = "queue_callr",
  inherit = queue,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    initialize_workers = function(workers, start) {
      super$initialize_workers(workers)
      if (!start) {
        return()
      }
      private$workers$handle <- replicate(
        workers,
        callr::r_session$new(
          wait = TRUE,
          options = callr::r_session_options(extra = list(supervise = TRUE))
        )
      )
    },
    worker_run = function(handle, worker, fun, args, task) {
      crew_catch_crash(handle$call(func = fun, args = args))
      handle
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
          task <- private$workers$task[index]
          result <- private$workers$handle[[index]]$read()
          private$add_result(task = task, result = result)
          private$workers$free[index] <- TRUE
          private$workers$sent[index] <- FALSE
          private$workers$done[index] <- FALSE
          private$workers$task[index] <- NA_character_
          private$workers$fun[index] <- list(NULL)
        }
      }
    },
    update_crashed = function() {
      up <- map_lgl(private$workers$worker, private$worker_up_log)
      if (!all(up)) {
        workers <- private$workers$worker[!up]
        crew_error(paste("crashed workers:", paste(workers, collapse = ", ")))
      }
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      timeout = 60,
      wait = 0.1,
      start = TRUE
    ) {
      private$timeout <- timeout
      private$wait <- wait
      private$initialize_tasks()
      private$initialize_results()
      private$initialize_workers(workers = workers, start = start)
      invisible()
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
