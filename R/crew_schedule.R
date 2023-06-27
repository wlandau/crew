#' @title Create a schedule for bookkeeping tasks.
#' @export
#' @family developer
#' @description Create an `R6` object to contain and manage task objects.
#' @details Not a user-side function. There are no examples. Please see
#'   [crew_controller_local()] for details.
#' @param seconds_interval Number of seconds between throttled iterations
#'   of task collection.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' schedule <- crew_schedule()
#' schedule$start()
#' schedule$push(task = mirai::mirai(1 + 1))
#' schedule$push(task = mirai::mirai(2 + 2))
#' Sys.sleep(4)
#' schedule$collect(throttle = FALSE)
#' schedule$pop()$data # numeric result
#' schedule$pop()$data # numeric result
#' schedule$pop()$data # NULL
#' }
crew_schedule <- function(seconds_interval = 0.25) {
  out <- crew_class_schedule$new(seconds_interval = seconds_interval)
  out$validate()
  out
}

#' @title Schedule class
#' @export
#' @family class
#' @description `R6` class to contain and manage task objects.
#' @details Not a user-side class. There are no examples. Please see
#'   [crew_controller_local()] for details.
crew_class_schedule <- R6::R6Class(
  classname = "crew_class_schedule",
  cloneable = FALSE,
  public = list(
    #' @field seconds_interval See [crew_schedule()].
    seconds_interval = NULL,
    #' @field pushed Hash table of pushed tasks.
    pushed = NULL,
    #' @field collected Linked list of resolved tasks with results available.
    collected = NULL,
    #' @field pushes Number of times a task has been pushed.
    pushes = NULL,
    #' @field head ID of the task at the head of the `collected` linked list.
    head = NULL,
    #' @field tail ID of the task at the tail of the `collected` linked list.
    tail = NULL,
    #' @field until Numeric of length 1, time point when
    #'   throttled task collection unlocks.
    until = NULL,
    #' @description Schedule constructor.
    #' @return An `R6` schedule object.
    #' @param seconds_interval See [crew_schedule()].
    initialize = function(seconds_interval = NULL) {
      self$seconds_interval <- seconds_interval
      invisible()
    },
    #' @description Validate the schedule.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(is.numeric(self$seconds_interval))
      crew_assert(self$pushed, is.null(.) || is.environment(.))
      crew_assert(self$collected, is.null(.) || is.environment(.))
      for (field in c("pushes", "until")) {
        crew_assert(
          self[[field]] %|||% 0,
          is.numeric(.),
          length(.) == 1L,
          !anyNA(.),
          is.finite(.),
          . >= 0
        )
      }
      crew_assert(
        self$head %|||% "head",
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.)
      )
      invisible()
    },
    #' @description Start the schedule.
    #' @details Sets the `pushed` and `collected` hash tables to new
    #'   empty environments.
    #' @return NULL (invisibly).
    start = function() {
      self$pushed <- new.env(hash = TRUE, parent = emptyenv())
      self$collected <- new.env(hash = TRUE, parent = emptyenv())
      self$pushes <- 0L
      self$head <- NULL
      self$tail <- NULL
      invisible()
    },
    #' @description Summarize the schedule.
    #' @return `NULL` if not started. Otherwise, a `tibble` with
    #'   the following columns:
    #'   * `pushed`: number tasks that were pushed but not collected yet.
    #'     These tasks may or may not have completed.
    #'   * `collected`: number of tasks that completed and were collected
    #'     but not yet retrieved with `pop()`.
    summary = function() {
      pushed <- .subset2(self, "pushed")
      collected <- .subset2(self, "collected")
      if (is.null(pushed)) {
        return(NULL)
      }
      tibble::tibble(pushed = length(pushed), collected = length(collected))
    },
    #' @description Push a task.
    #' @details Add a task to the `pushed` hash table
    #' @return `NULL` (invisibly).
    #' @param task The `mirai` task object to push.
    push = function(task) {
      index <- .subset2(self, "pushes") + 1L
      self$pushes <- index
      pushed <- .subset2(self, "pushed")
      pushed[[as.character(index)]] <- task
      invisible()
    },
    #' @description Throttle repeated calls.
    #' @return `TRUE` to throttle, `FALSE` to continue.
    throttle = function() {
      now <- nanonext::mclock()
      if (is.null(self$until)) {
        self$until <- now + (1000 * self$seconds_interval)
      }
      if (now < self$until) {
        return(TRUE)
      } else {
        self$until <- NULL
        return(FALSE)
      }
    },
    #' @description Collect resolved tasks.
    #' @details Scan the tasks in `pushed` and move the resolved ones to the
    #'   head of the `collected` linked list.
    #' @return `NULL` (invisibly).
    #' @param throttle whether to defer task collection
    #'   until the next task collection request at least
    #'   `seconds_interval` seconds from the original request.
    #'   The idea is similar to `shiny::throttle()` except that `crew` does not
    #'   accumulate a backlog of requests. The technique improves robustness
    #'   and efficiency.
    collect = function(throttle = FALSE) {
      if (throttle && self$throttle()) {
        return(invisible())
      }
      pushed <- .subset2(self, "pushed")
      collected <- .subset2(self, "collected")
      index_unresolved <- lapply(X = pushed, FUN = .unresolved)
      index_resolved <- !as.logical(index_unresolved)
      which_resolved <- names(index_unresolved)[index_resolved]
      if (length(which_resolved) < 1L) {
        return(invisible())
      }
      if (is.null(.subset2(self, "head"))) {
        self$head <- which_resolved[1L]
      }
      tail <- .subset2(self, "tail")
      for (id in which_resolved) {
        if (!is.null(tail)) {
          collected[[tail]]$head <- id
        }
        collected[[id]] <- list(task = .subset2(pushed, id))
        tail <- id
      }
      self$tail <- tail
      rm(list = which_resolved, envir = pushed)
      invisible()
    },
    #' @description List collected tasks.
    #' @details Exists to support a `purrr`-like extension to `crew` for
    #'   functional programming. For developers only. Not supported
    #'   for controller groups.
    #' @return A list of monad objects from individual tasks.
    list = function() {
      map(self$collected, ~.subset2(.subset2(.x, "task"), "data"))
    },
    #' @description Pop a task from the `collected` linked list.
    #' @return A task object if available, `NULL` otherwise.
    pop = function() {
      head <- .subset2(self, "head")
      if (is.null(head)) {
        return(NULL)
      }
      collected <- .subset2(self, "collected")
      result <- collected[[head]]
      rm(list = head, envir = collected)
      new_head <- .subset2(result, "head")
      self$head <- new_head
      if (is.null(new_head)) {
        self$tail <- NULL
      }
      .subset2(result, "task")
    },
    #' @description Check if the schedule is empty.
    #' @return `TRUE` if the `pushed` and `collected` hash tables are both
    #'   empty, `FALSE` otherwise.
    empty = function() {
      is.null(.subset2(self, "head")) && length(.subset2(self, "pushed")) < 1L
    },
    #' @description Check if the schedule is nonempty.
    #' @return `TRUE` if either `pushed` or `collected` is nonempty,
    #'   `FALSE` otherwise.
    nonempty = function() {
      (!is.null(.subset2(self, "head"))) ||
        length(.subset2(self, "pushed")) > 0L
    },
    #' @description Check if all previously pushed tasks are now collected.
    #' @return `TRUE` if all previously pushed tasks are now collected,
    #'   `FALSE` otherwise. Could be `FALSE` if there are unresolved tasks
    #'   or there are no tasks at all.
    collected_all = function() {
      length(.subset2(self, "pushed")) < 1L
    },
    #' @description Check if there is at least one collected task or
    #'   there are no pushed tasks.
    #' @return `TRUE` if there is at least one collected task or
    #'   there are no pushed tasks, `FALSE` otherwise.
    collected_one = function() {
      (!is.null(.subset2(self, "head"))) ||
        (length(.subset2(self, "pushed")) < 1L)
    },
    #' @description Either `collected_all()` or `collected_one()`, depending
    #'   on the `mode` argument.
    #' @return `TRUE` or `FALSE`, depending on `mode` and the state of the
    #'   schedule.
    #' @param mode `"all"` to call `collected_all()` or `"one"` to call
    #'   `collected_one()`.
    collected_mode = function(mode = "all") {
      if_any(
        identical(mode, "all"),
        self$collected_all(),
        self$collected_one()
      )
    }
  )
)
