#' @title Create a `crew` relay object.
#' @export
#' @family developer
#' @description Create an `R6` `crew` relay object.
#' @details A `crew` relay object keeps track of tasks that are resolved
#'   but not yet popped.
#' @return An `R6` `crew` relay object.
#' @examples
#' crew_relay()
crew_relay <- function() {
  relay <- crew_class_relay$new()
  relay$validate()
  relay
}

#' @title `R6` relay class.
#' @export
#' @family class
#' @description `R6` class for relay configuration.
#' @details See [crew_relay()].
#' @examples
#' crew_relay()
crew_class_relay <- R6::R6Class(
  classname = "crew_class_relay",
  cloneable = FALSE,
  public = list(
    #' @field condition Main condition variable.
    condition = NULL,
    #' @field from Condition variable that `self$condition` receives
    #'   signals from.
    from = NULL,
    #' @field to Condition variable that `self$condition` forwards
    #'   signals to.
    to = NULL,
    #' @field unpopped Number of observed unpopped tasks.
    unpopped = NULL,
    #' @field popped Number of observed popped tasks.
    popped = NULL,
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    validate = function() {
      for (field in c("unpopped", "popped")) {
        crew_assert(
          self[[field]] %|||% 0L,
          length(.) == 1L,
          is.integer(.),
          !anyNA(.),
          . >= 0L
        )
      }
      for (field in c("condition", "from", "to")) {
        if (!is.null(self[[field]])) {
          crew_assert(inherits(self[[field]], "conditionVariable"))
        }
      }
      invisible()
    },
    #' @description Start the relay object.
    #' @return `NULL` (invisibly).
    start = function() {
      if (is.null(self$condition)) {
        self$condition <- nanonext::cv()
        if (!is.null(self$from)) {
          nanonext::`%~>%`(cv = self$from, cv2 = self$condition)
        }
        if (!is.null(self$to)) {
          nanonext::`%~>%`(cv = self$condition, cv2 = self$to)
        }
        self$unpopped <- 0L
        self$popped <- 0L
      }
      invisible()
    },
    #' @description Terminate the relay object.
    #' @return `NULL` (invisibly).
    terminate = function() {
      self$condition <- NULL
      self$unpopped <- NULL
      self$popped <- NULL
      invisible()
    },
    #' @description Count the total count of resolved tasks.
    #' @return Positive integer of length 1, total number of resolved tasks.
    resolved = function() {
      observed <- .subset2(self, "unpopped") + .subset2(self, "popped")
      observed + nanonext::cv_value(.subset2(self, "condition"))
    },
    #' @description Register a popped task.
    #' @return `NULL` (invisibly).
    #' @param n Number of tasks to register as popped.
    pop = function(n = 1L) {
      wait_condition <- .subset2(self, "wait_condition")
      replicate(n, wait_condition(seconds_timeout = 0))
      crew_relay_assert_unpopped(.subset2(self, "unpopped"), n = n)
      on.exit({
        self$unpopped <- .subset2(self, "unpopped") - n
        self$popped <- .subset2(self, "popped") + n
      })
      invisible()
    },
    #' @description Wait until an unobserved task resolves or the timeout
    #'   is reached.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    wait_condition = function(seconds_timeout = 1e9) {
      timeout <- seconds_timeout * 1000
      condition <- .subset2(self, "condition")
      result <- FALSE
      on.exit(self$unpopped <- .subset2(self, "unpopped") + as.integer(result))
      result <- nanonext::until(cv = condition, msec = timeout)
      invisible()
    },
    #' @description Wait until at least one task is available to be popped.
    #' @return `TRUE` if at least one task is available after waiting,
    #'   `FALSE` otherwise.
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    wait_unpopped = function(seconds_timeout = 1e9) {
      if (.subset2(self, "unpopped") < 1L) {
        .subset2(self, "wait_condition")(seconds_timeout = seconds_timeout)
      }
      .subset2(self, "unpopped") > 0L
    },
    #' @description Wait for until the number of
    #'   resolved tasks reaches a specified number.
    #' @return `TRUE` if the number of
    #'   resolved tasks reaches a specified number after waiting,
    #'   `FALSE` otherwise
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    #' @param resolved Positive integer of length 1. This method waits
    #'   until the number of resolved tasks reaches this value or above.
    wait_resolved = function(seconds_timeout = 1e9, resolved = 1L) {
      if (.subset2(self, "resolved")() < resolved) {
        .subset2(self, "wait_condition")(seconds_timeout = seconds_timeout)
      }
      .subset2(self, "resolved")() >= resolved
    }
  )
)

crew_relay_assert_unpopped <- function(unpopped, n) {
  if (unpopped < n) {
    crew_error(
      message = paste0(
        "Trying to pop but the unpopped task resolution count is less than ",
        n,
        ". Please send a bug report with a reprex to ",
        "https://github.com/wlandau/crew/issues"
      )
    )
  }
}
