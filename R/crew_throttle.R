#' @title Create a stateful throttling object.
#' @export
#' @family throttle
#' @description Create an `R6` object for throttling.
#' @details Throttling is a technique that limits how often a function is
#'   called in a given period of time. [crew_throttle()] objects support
#'   the `throttle` argument of controller methods, which ensures auto-scaling
#'   only happen every `seconds_interval` seconds. This helps avoid
#'   overburdening the `mirai` dispatcher and other resources.
#' @return An `R6` object with throttle configuration settings and methods.
#' @param seconds_interval Positive numeric of length 1, throttling interval.
#'   The `poll()` method returns `TRUE` if and only if it was not called
#'   in the last `seconds_interval` seconds.
#' @examples
#' throttle <- crew_throttle(seconds_interval = 0.5)
#' throttle$poll()
#' throttle$poll()
crew_throttle <- function(
  seconds_interval = 0.5
) {
  throttle <- crew_class_throttle$new(seconds_interval = seconds_interval)
  throttle$validate()
  throttle
}

#' @title `R6` throttle class.
#' @export
#' @family throttle
#' @description `R6` class for throttle configuration.
#' @details See [crew_throttle()].
#' @examples
#' throttle <- crew_throttle(seconds_interval = 0.5)
#' throttle$poll()
#' throttle$poll()
crew_class_throttle <- R6::R6Class(
  classname = "crew_class_throttle",
  cloneable = FALSE,
  private = list(
    .seconds_interval = NULL,
    .polled = NULL
  ),
  active = list(
    #' @field seconds_interval Positive numeric of length 1,
    #'  throttling interval in seconds.
    seconds_interval = function() {
      .subset2(private, ".seconds_interval")
    },
    #' @field polled Positive numeric of length 1,
    #'  millisecond timestamp of the last time `poll()` returned `TRUE`.
    #'  `NULL` if `poll()` was never called on the current object.
    polled = function() {
      .subset2(private, ".polled")
    }
  ),
  public = list(
    #' @description Throttle constructor.
    #' @return An `R6` object with throttle configuration.
    #' @param seconds_interval Throttling interval in seconds.
    #' @examples
    #' throttle <- crew_throttle(seconds_interval = 0.5)
    #' throttle$poll()
    #' throttle$poll()
    initialize = function(
      seconds_interval = NULL
    ) {
      private$.seconds_interval <- seconds_interval
    },
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        private$.seconds_interval,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.),
        is.finite(.),
        . > 0,
        message = paste(
          "crew_throttle() argument seconds_interval",
          "must be a finite positive numeric of length 1."
        )
      )
      invisible()
    },
    #' @description Poll the throttler.
    #' @return `TRUE` if `poll()` did not return `TRUE` in the last
    #'   `seconds_interval` seconds, `FALSE` otherwise.
    poll = function() {
      now <- nanonext::mclock()
      last <- private$.polled %|||% -Inf
      interval <- 1000 * private$.seconds_interval
      out <- (now - last) > interval
      if (out) {
        private$.polled <- now
      }
      out
    },
    #' @description Reset the throttle object so the next `poll()` returns
    #'   `TRUE`.
    #' @return `NULL` (invisibly).
    reset = function() {
      private$.polled <- NULL
      invisible()
    }
  )
)
