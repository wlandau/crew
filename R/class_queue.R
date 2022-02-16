#' @title Task queue class
#' @export
#' @description A task queue for short-lived worker management tasks
#'   such as launching and polling.
#' @description Adapted from
#'   <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#'   under the MIT license. See also the `crew` package `NOTICE` file.
class_queue <- R6::R6Class(
  classname = "queue",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    next_id = 1L,
    get_next_id = function() {
      id <- private$next_id
      private$next_id <- id + 1L
      paste0(".", id)
    },
    start_workers = function(workers) {
      private$tasks <- tibble::tibble(
        id = character(),
        idle = logical(),
        state = character(0),
        fun = list(),
        args = list(),
        worker = list(),
        result = list()
      )
      for (index in seq_len(workers)) {
        handle <- callr::r_session$new(wait = FALSE)
        private$tasks <- tibble::add_row(
          private$tasks,
          id = paste0(".idle-", index),
          idle = TRUE,
          state = "running",
          fun = list(NULL),
          args = list(NULL),
          worker = list(handle),
          result = list(NULL)
        )
      }
    },
    schedule = function() {
      ready <- which(private$tasks$state == "ready")
      if (!length(ready)) {
        return()
      }
      handles <- private$tasks$worker[ready]
      private$tasks$result[ready] <- lapply(handles, function(x) x$read())
      private$tasks$worker[ready] <- replicate(length(ready), NULL)
      private$tasks$state[ready] <- ifelse(
        private$tasks$idle[ready],
        "waiting",
        "done"
      )
      waiting <- which(private$tasks$state == "waiting")[seq_along(ready)]
      private$tasks$worker[waiting] <- handles
      private$tasks$state[waiting] <- ifelse(
        private$tasks$idle[waiting],
        "ready",
        "running"
      )
      lapply(waiting, function(index) {
        if (!private$tasks$idle[index]) {
          private$tasks$worker[[index]]$call(
            private$tasks$fun[[index]],
            private$tasks$args[[index]]
          )
        }
      })
    },
    poll = function(timeout = 0) {
      limit <- Sys.time() + timeout
      as_ms <- function(x) {
        if (x == Inf) {
          -1
        } else {
          as.integer(as.double(x, "secs") * 1000)
        }
      }
      repeat{
        to_poll <- which(private$tasks$state == "running")
        connections <- lapply(
          private$tasks$worker[to_poll],
          function(x) {
            x$get_poll_connection()
          }
        )
        result <- processx::poll(connections, as_ms(timeout))
        private$tasks$state[to_poll][result == "ready"] <- "ready"
        private$schedule()
        out <- private$tasks$id[private$tasks$state == "done"]
        if (is.finite(timeout)) {
          timeout <- limit - Sys.time()
        }
        if (length(out) || timeout < 0) {
          break;
        }
      }
      out
    }
  ),
  public = list(
    #' @field tasks Data frame of tasks.
    tasks = NULL,
    #' @description Task queue constructor.
    #' @return The `new()` method calls the constructor
    #'   and returns a task queue object.
    #' @param workers Number of local processes that
    #'   iterate through tasks.
    initialize = function(workers = 4L) {
      private$start_workers(workers)
      invisible()
    },
    #' @description Push a task to the queue.
    #' @return `NULL` (invsibly)
    #' @param fun Function to run for the task.
    #' @param args Arguments to `fun`.
    push = function(fun, args = list()) {
      id <- private$get_next_id()
      before <- which(private$tasks$idle)[1]
      private$tasks <- tibble::add_row(
        .data = private$tasks,
        .before = before,
        id = id,
        idle = FALSE,
        state = "waiting",
        fun = list(fun),
        args = list(args),
        worker = list(NULL),
        result = list(NULL)
      )
      private$schedule()
      invisible()
    },
    #' @description Poll the workers and pop a result off the queue
    #'   if available.
    #' @return The result of a done task if available, `NULL` if there
    #'   are no done tasks. The return value is a list with the return
    #'   value of the function and the task ID.
    #' @param timeout Number of seconds of timeout for polling.
    pop = function(timeout = 0) {
      if (is.na(done <- self$poll(timeout)[1])) {
        return(NULL)
      }
      row <- match(done, private$tasks$id)
      result <- private$tasks$result[[row]]
      private$tasks <- private$tasks[-row, ]
      list(value = result, is = done)
    }
  )
)
