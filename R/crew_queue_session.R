#' @title Local session queue
#' @export
#' @aliases crew_queue_session
#' @family queue
#' @description Task queue with `callr::r_session` workers.
#' @details Some public methods are inherited from [crew_queue].
#'   See the [crew_queue] help file for details on those.
#'
#'   The session queue is a slightly enhanced version of
#'   <https://www.tidyverse.org/blog/2019/09/callr-task-q/>,
#'   with the ability to detect crashes and block the R session
#'   until the backlog clears. See the "Queue attribution"
#'   section, the `crew` `README.md` file, and the `crew` `NOTICE` file
#'   for attribution.
#' @inheritSection crew_queue Queue attribution
#' @examples
#' fun <- function(x) x + 1
#' args <- list(x = 1)
#' queue <- crew_queue_session$new(timeout = 60, wait = 0.1)
#' queue$push(fun = fun, args = args)
#' queue$block()
#' result <- queue$pop()
#' str(result)
#' result$result$result
#' queue$shutdown()
#' processx::supervisor_kill()
crew_queue_session <- R6::R6Class(
  classname = "crew_queue_session",
  inherit = crew_queue,
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
    #' @description Local session queue constructor.
    #' @return A local session queue object.
    #' @param workers Number of workers in the queue.
    #' @param timeout Number of seconds to for a worker to wait
    #'   for something to happen (e.g. worker initialization)
    #'   before timing out and quitting.
    #' @param wait Number of seconds to wait in between iterations while
    #'   waiting for something to happen (e.g. worker initialization).
    initialize = function(
      workers = 1,
      timeout = 60,
      wait = 0.1
    ) {
      crew_true(workers, is.numeric(.), is.finite(.), length(.) == 1, . > 0)
      crew_true(timeout, is.numeric(.), !anyNA(.), length(.) == 1, . >= 0)
      crew_true(wait, is.numeric(.), !anyNA(.), length(.) == 1, . >= 0)
      private$timeout <- timeout
      private$wait <- wait
      private$initialize_tasks()
      private$initialize_results()
      private$initialize_workers(workers = workers)
      invisible()
    },
    #' @description Push a new task on to the queue.
    #' @return `NULL` (invisibly)
    #' @param fun R function that runs the task.
    #' @param args Named list of arguments to `fun`.
    #' @param task Character of length 1 with the task ID.
    #' @param update Logical of length 1, whether to update the
    #'   internal state of the queue after pushing. See the
    #'   `update()` method for details.
    push = function(
      fun,
      args = list(),
      task = crew_name(),
      update = TRUE
    ) {
      super$push(fun = fun, args = args, task = task, update = update)
    },
    #' @description Shut down the workers.
    #' @return `NULL` (invisibly)
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
