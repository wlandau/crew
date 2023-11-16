#' @title Create a `crew` stage object.
#' @export
#' @family developer
#' @description Create an `R6` `crew` stage object.
#' @details A `crew` stage object keeps track of tasks that are resolved
#'   but not yet popped.
#' @return An `R6` `crew` stage object.
#' @examples
#' crew_stage()
crew_stage <- function() {
  stage <- crew_class_stage$new()
  stage$validate()
  stage
}

#' @title `R6` stage class.
#' @export
#' @family class
#' @description `R6` class for stage configuration.
#' @details See [crew_stage()].
#' @examples
#' crew_stage()
crew_class_stage <- R6::R6Class(
  classname = "crew_class_stage",
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
    #' @description Initialize the stage object.
    #' @return `NULL` (invisibly).
    start = function() {
      self$condition <- nanonext::cv()
      self$unpopped <- 0L
      self$popped <- 0L
      invisible()
    },
    #' @description Inherit signals from a different condition variable.
    #' @return `NULL` (invisibly).
    #' @param condition A `nanonext` condition variable which will forward
    #'  signals to `self$condition`.
    inherit = function(condition) {
      nanonext::`%~>%`(cv = condition, cv2 = self$condition)
      invisible()
    },
    #' @description Count the total count of resolved tasks.
    #' @return Positive integer of length 1, total number of resolved tasks.
    resolved = function() {
      unobserved <- nanonext::cv_value(.subset2(self, "condition"))
      unobserved + .subset2(self, "unpopped") + .subset2(self, "popped")
    },
    #' @description Register a popped task.
    #' @return `NULL` (invisibly).
    pop = function() {
      .subset2(self, "wait_condition")(
        seconds_timeout = 0,
        seconds_interval = 0
      )
      if (.subset2(self, "unpopped") < 1L) {
        crew_error(
          message = paste(
            "Trying to pop but the unpopped task resolution count is 0.",
            "Please send a bug report with a reprex to",
            "https://github.com/wlandau/crew/issues"
          )
        )
      }
      self$unpopped <- .subset2(self, "unpopped") - 1L
      self$popped <- .subset2(self, "popped") + 1L
      invisible()
    },
    #' @description Wait until an unobserved task resolves or the timeout
    #'   is reached.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    #' @param seconds_interval Positive numeric of length 1. Waiting
    #'   is chopped into intervals which can be interrupted by task
    #'   resolution signals but not by CTRL-C. `seconds_interval`
    #'   is the length of each interval. Long intervals are kind to the CPU
    #'   but may delay the effect of CTRL-C by at most `seconds_interval`.
    wait_condition = function(seconds_timeout = Inf, seconds_interval = 0.5) {
      timeout <- seconds_timeout * 1000
      interval <- seconds_interval * 1000
      condition <- .subset2(self, "condition")
      result <- FALSE
      on.exit(self$unpopped <- .subset2(self, "unpopped") + as.integer(result))
      now <- nanonext::mclock()
      while (!(result || (nanonext::mclock() - now > timeout))) {
        result <- nanonext::until(cv = condition, msec = interval)
      }
      invisible()
    },
    #' @description Wait until at least one task is available to be popped.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    #' @param seconds_interval Positive numeric of length 1. Waiting
    #'   is chopped into intervals which can be interrupted by task resolution
    #'   signals but not by CTRL-C. `seconds_interval`
    #'   is the length of each interval. Long intervals are kind to the CPU
    #'   but may delay the effect of CTRL-C by at most `seconds_interval`.
    wait_unpopped = function(seconds_timeout = Inf, seconds_interval = 0.5) {
      if (.subset2(self, "unpopped") < 1L) {
        self$wait_condition(
          seconds_timeout = seconds_timeout,
          seconds_interval = seconds_interval
        )
      }
      invisible()
    },
    #' @description Wait for until the number of
    #'   resolved tasks reaches a specified number.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    #' @param seconds_interval Positive numeric of length 1. Waiting
    #'   is chopped into intervals which can be interrupted by task resolution
    #'   signals but not by CTRL-C. `seconds_interval`
    #'   is the length of each interval. Long intervals are kind to the CPU
    #'   but may delay the effect of CTRL-C by at most `seconds_interval`.
    #' @param resolved Positive integer of length 1. This method waits
    #'   until the number of resolved tasks reaches this value or above.
    wait_resolved = function(
      seconds_timeout = Inf,
      seconds_interval = 0.5,
      resolved = 1L
    ) {
      if (.subset2(self, "resolved")() < resolved) {
        self$wait_condition(
          seconds_timeout = seconds_timeout,
          seconds_interval = seconds_interval
        )
      }
      invisible()
    }
  )
)
