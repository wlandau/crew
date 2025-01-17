#' @title Create a stateful throttling object.
#' @export
#' @family throttle
#' @description Create an `R6` object for throttling.
#' @details Throttling is a technique that limits how often a function is
#'   called in a given period of time. [crew_throttle()] objects support
#'   the `throttle` argument of controller methods, which ensures auto-scaling
#'   does not induce superfluous overhead.
#'   The throttle uses deterministic exponential backoff algorithm
#'   (<https://en.wikipedia.org/wiki/Exponential_backoff>) which
#'   increases wait times when there is nothing to do and decreases
#'   wait times when there is something to do. The controller decreases
#'   or increases the wait time with methods `accelerate()` and `decelerate()`
#'   in the throttle object, respectively,
#'   by dividing or multiplying by `base` (but keeping the wait time
#'   between `seconds_min` and `seconds_max`).
#'   In practice, `crew` calls `reset()` instead of `update()`
#'   in order to respond quicker to surges of activity (see the
#'   `update()` method).
#' @return An `R6` object with throttle configuration settings and methods.
#' @param seconds_max Positive numeric scalar, maximum throttling interval
#    in seconds.
#' @param seconds_min Positive numeric scalar, minimum throttling interval.
#    in seconds.
#' @param seconds_start Positive numeric scalar,
#'   the initial wait time interval in seconds.
#'   The default is `min` because there is almost always
#'   auto-scaling to be done when the controller is created.
#'   `reset()` always sets the current wait interval back to `seconds_start`.
#' @param base Numeric scalar greater than 1, base of the exponential
#'   backoff algorithm. `increment()` multiplies the waiting interval by
#'   `base` and `decrement()` divides the waiting interval by `base`.
#'   The default `base` is 2, which specifies a binary exponential
#'   backoff algorithm.
#' @examples
#' throttle <- crew_throttle(seconds_max = 1)
#' throttle$poll()
#' throttle$poll()
crew_throttle <- function(
  seconds_max = 1,
  seconds_min = 1e-3,
  seconds_start = seconds_min,
  base = 2
) {
  throttle <- crew_class_throttle$new(
    seconds_max = seconds_max,
    seconds_min = seconds_min,
    seconds_start = seconds_start,
    base = base
  )
  throttle$reset()
  throttle$validate()
  throttle
}

#' @title `R6` throttle class.
#' @export
#' @family throttle
#' @description `R6` class for throttle configuration.
#' @details See [crew_throttle()].
#' @examples
#' throttle <- crew_throttle(seconds_max = 1)
#' throttle$poll()
#' throttle$poll()
crew_class_throttle <- R6::R6Class(
  classname = "crew_class_throttle",
  cloneable = FALSE,
  private = list(
    .seconds_max = NULL,
    .seconds_min = NULL,
    .seconds_start = NULL,
    .base = NULL,
    .seconds_interval = 1e-3,
    .polled = -Inf
  ),
  active = list(
    #' @field seconds_max See [crew_throttle()].
    seconds_max = function() {
      .subset2(private, ".seconds_max")
    },
    #' @field seconds_min See [crew_throttle()].
    seconds_min = function() {
      .subset2(private, ".seconds_min")
    },
    #' @field seconds_start See [crew_throttle()].
    seconds_start = function() {
      .subset2(private, ".seconds_start")
    },
    #' @field base See [crew_throttle()].
    base = function() {
      .subset2(private, ".base")
    },
    #' @field seconds_interval Current wait time interval.
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
    #' @param seconds_max See [crew_throttle()].
    #' @param seconds_min See [crew_throttle()].
    #' @param seconds_start See [crew_throttle()].
    #' @param base See [crew_throttle()].
    #' @examples
    #' throttle <- crew_throttle(seconds_max = 1)
    #' throttle$poll()
    #' throttle$poll()
    initialize = function(
      seconds_max = NULL,
      seconds_min = NULL,
      seconds_start = NULL,
      base = NULL
    ) {
      private$.seconds_max <- seconds_max
      private$.seconds_min <- seconds_min
      private$.seconds_start <- seconds_start
      private$.base <- base
    },
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    validate = function() {
      for (field in c(names(formals(crew_throttle)), "seconds_interval")) {
        crew_assert(
          self[[field]],
          is.numeric(.),
          length(.) == 1L,
          !anyNA(.),
          . > 0,
          message = paste(
            "crew throttle",
            field,
            "must be a positive numeric scalar."
          )
        )
      }
      crew_assert(
        self$base > 1,
        message = "crew throttle base must be greater than 1."
      )
      crew_assert(
        self$polled,
        is.numeric(.),
        length(.) == 1L,
        !anyNA(.),
        message = c(
          "crew throttle 'polled' must be a non-missing numeric scalar."
        )
      )
      crew_assert(
        self$seconds_min <= self$seconds_max,
        message = paste(
          "crew throttle seconds_min must be",
          "less than or equal to seconds_max."
        )
      )
      for (field in c("seconds_start", "seconds_interval")) {
        crew_assert(
          self$seconds_min <= self[[field]] &&
            self[[field]] <= self$seconds_max,
          message = paste(
            "crew throttle",
            field,
            "must be between seconds_min and seconds_max."
          )
        )
      }
      invisible()
    },
    #' @description Poll the throttler.
    #' @return `TRUE` if `poll()` did not return `TRUE` in the last
    #'   `max` seconds, `FALSE` otherwise.
    poll = function() {
      now <- now()
      polled <- .subset2(private, ".polled")
      interval <- .subset2(private, ".seconds_interval")
      out <- (now - polled) > interval
      if (out) {
        private$.polled <- now
      }
      out
    },
    #' @description Divide `seconds_interval` by `base`.
    #' @return `NULL` (invisibly). Called for its side effects.
    accelerate = function() {
      old <- .subset2(private, ".seconds_interval")
      base <- .subset2(private, ".base")
      min <- .subset2(private, ".seconds_min")
      private$.seconds_interval <- max(min, old / base)
      invisible()
    },
    #' @description Multiply `seconds_interval` by `base`.
    #' @return `NULL` (invisibly). Called for its side effects.
    decelerate = function() {
      old <- .subset2(private, ".seconds_interval")
      base <- .subset2(private, ".base")
      max <- .subset2(private, ".seconds_max")
      private$.seconds_interval <- min(max, old * base)
      invisible()
    },
    #' @description Reset the throttle object so the next `poll()` returns
    #'   `TRUE`, and reset the wait time interval to its initial value.
    #' @return `NULL` (invisibly).
    reset = function() {
      private$.seconds_interval <- .subset2(private, ".seconds_start")
      private$.polled <- -Inf
      invisible()
    },
    #' @description Reset the throttle when there is activity and
    #'   decelerate it gradually when there is no activity.
    #' @return `NULL` (invisibly).
    #' @param activity `TRUE` if there is activity, `FALSE` otherwise.
    update = function(activity) {
      if (activity) {
        .subset2(self, "reset")()
      } else {
        .subset2(self, "decelerate")()
      }
      invisible()
    }
  )
)
