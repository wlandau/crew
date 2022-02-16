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
    add_worker = function(index) {
      self$tasks <- tibble::add_row(
        self$tasks,
        id = paste0(".idle-", index),
        idle = TRUE,
        state = "running",
        fun = list(NULL),
        args = list(NULL),
        worker = list(callr::r_session$new(wait = FALSE)),
        result = list(NULL)
      )
    },
    start_workers = function(workers) {
      self$tasks <- tibble::tibble(
        id = character(),
        idle = logical(),
        state = character(0),
        fun = list(),
        args = list(),
        worker = list(),
        result = list()
      )
      lapply(workers, private$add_worker)
      invisible()
    },
    schedule = function() {
      ready <- which(self$tasks$state == "ready")
      if (!length(ready)) {
        return()
      }
      handles <- self$tasks$worker[ready]
      self$tasks$result[ready] <- lapply(handles, function(x) x$read())
      self$tasks$worker[ready] <- replicate(length(ready), NULL)
      self$tasks$state[ready] <- ifelse(
        self$tasks$idle[ready],
        "waiting",
        "done"
      )
      waiting <- which(self$tasks$state == "waiting")[seq_along(ready)]
      self$tasks$worker[waiting] <- handles
      self$tasks$state[waiting] <- ifelse(
        self$tasks$idle[waiting],
        "ready",
        "running"
      )
      lapply(waiting, function(index) {
        if (!self$tasks$idle[index]) {
          self$tasks$worker[[index]]$call(
            self$tasks$fun[[index]],
            self$tasks$args[[index]]
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
        to_poll <- which(self$tasks$state == "running")
        connections <- lapply(
          self$tasks$worker[to_poll],
          function(x) {
            x$get_poll_connection()
          }
        )
        result <- processx::poll(connections, as_ms(timeout))
        self$tasks$state[to_poll][result == "ready"] <- "ready"
        private$schedule()
        out <- self$tasks$id[self$tasks$state == "done"]
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
    #' @return The ID of the assigned task.
    #' @param fun Function to run for the task.
    #' @param args Arguments to `fun`.
    push = function(fun, args = list()) {
      id <- private$get_next_id()
      before <- which(self$tasks$idle)[1]
      self$tasks <- tibble::add_row(
        .data = self$tasks,
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
      id
    },
    #' @description Poll the workers and pop a result off the queue
    #'   if available.
    #' @return The result of a done task (a list) if available, `NULL` if
    #'   there are no done tasks. Contains a list of `callr` output fields
    #'   and the task ID.
    #' @param timeout Number of seconds of timeout for polling.
    pop = function(timeout = 0) {
      if (is.na(done <- private$poll(timeout)[1])) {
        return(NULL)
      }
      row <- match(done, self$tasks$id)
      result <- self$tasks$result[[row]]
      self$tasks <- self$tasks[-row, ]
      c(result, task_id = done)
    },
    #' @description Terminate all workers. The queue is no longer usable
    #'   after termination.
    #' @return `NULL` (invisibly).
    terminate = function() {
      for (worker in self$tasks$worker) {
        if (!is.null(worker)) {
          worker$kill()
        }
      }
      invisible()
    }
  )
)
