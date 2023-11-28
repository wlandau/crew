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
    #' @field condition `nanonext` condition variable that inherits
    #'   signals from `mirai::nextget("cv")`.
    condition = NULL,
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
      if (!is.null(self$condition)) {
        crew_assert(inherits(self$condition, "conditionVariable"))
      }
      invisible()
    },
    #' @description Initialize the relay object.
    #' @return `NULL` (invisibly).
    start = function() {
      self$condition <- nanonext::cv()
      self$unpopped <- 0L
      self$popped <- 0L
      invisible()
    },
    #' @description Forward signals from a different condition variable.
    #' @return `NULL` (invisibly).
    #' @param condition A `nanonext` condition variable which will forward
    #'  signals to `self$condition`.
    from = function(condition) {
      nanonext::`%~>%`(cv = condition, cv2 = self$condition)
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
    pop = function() {
      .subset2(self, "wait_condition")(seconds_timeout = 0)
      crew_relay_assert_unpopped(.subset2(self, "unpopped"))
      on.exit({
        self$unpopped <- .subset2(self, "unpopped") - 1L
        self$popped <- .subset2(self, "popped") + 1L
      })
      invisible()
    },
    #' @description Wait until an unobserved task resolves or the timeout
    #'   is reached.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    wait_condition = function(seconds_timeout = Inf) {
      timeout <- seconds_timeout * 1000
      condition <- .subset2(self, "condition")
      result <- FALSE
      on.exit(self$unpopped <- .subset2(self, "unpopped") + as.integer(result))
      result <- nanonext::until_(cv = condition, msec = timeout)
      invisible()
    },
    #' @description Wait until at least one task is available to be popped.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    wait_unpopped = function(seconds_timeout = Inf) {
      if (.subset2(self, "unpopped") < 1L) {
        .subset2(self, "wait_condition")(seconds_timeout = seconds_timeout)
      }
      invisible()
    },
    #' @description Wait for until the number of
    #'   resolved tasks reaches a specified number.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    #' @param resolved Positive integer of length 1. This method waits
    #'   until the number of resolved tasks reaches this value or above.
    wait_resolved = function(seconds_timeout = Inf, resolved = 1L) {
      if (.subset2(self, "resolved")() < resolved) {
        .subset2(self, "wait_condition")(seconds_timeout = seconds_timeout)
      }
      invisible()
    }
  )
)

crew_relay_assert_unpopped <- function(unpopped) {
  if (unpopped < 1L) {
    crew_error(
      message = paste(
        "Trying to pop but the unpopped task resolution count is 0.",
        "Please send a bug report with a reprex to",
        "https://github.com/wlandau/crew/issues"
      )
    )
  }
}
