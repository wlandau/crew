#' @title Create a `crew` relay object.
#' @export
#' @family relay
#' @description Create an `R6` `crew` relay object.
#' @details A `crew` relay object keeps the signaling relationships
#'   among condition variables.
#' @return An `R6` `crew` relay object.
#' @param throttle A [crew_throttle()] object.
#' @examples
#' crew_relay()
crew_relay <- function(throttle = crew_throttle()) {
  relay <- crew_class_relay$new(throttle = throttle)
  relay$validate()
  relay
}

#' @title `R6` relay class.
#' @export
#' @family relay
#' @description `R6` class for relay configuration.
#' @details See [crew_relay()].
#' @examples
#' crew_relay()
crew_class_relay <- R6::R6Class(
  classname = "crew_class_relay",
  cloneable = FALSE,
  portable = FALSE,
  private = list(
    .condition = NULL,
    .from = NULL,
    .to = NULL,
    .throttle = NULL
  ),
  active = list(
    #' @field condition Main condition variable.
    condition = function() {
      .condition
    },
    #' @field from Condition variable to relay from.
    from = function() {
      .from
    },
    #' @field to Condition variable to relay to.
    to = function() {
      .to
    },
    #' @field throttle A [crew_throttle()] object for `wait()`.
    throttle = function() {
      .throttle
    }
  ),
  public = list(
    #' @description Relay constructor.
    #' @return A [crew_relay()] object.
    #' @param throttle A [crew_throttle()] object.
    initialize = function(throttle) {
      .throttle <<- throttle
    },
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    validate = function() {
      for (field in c(".condition", ".from", ".to")) {
        if (!is.null(private[[field]])) {
          crew_assert(inherits(private[[field]], "conditionVariable"))
        }
      }
      .throttle$validate()
      invisible()
    },
    #' @description Start the relay object.
    #' @return `NULL` (invisibly).
    start = function() {
      if (is.null(.condition)) {
        .condition <<- nanonext::cv()
        if (!is.null(.from)) {
          nanonext::`%~>%`(cv = .from, cv2 = .condition)
        }
        if (!is.null(.to)) {
          nanonext::`%~>%`(cv = .condition, cv2 = .to)
        }
      }
      invisible()
    },
    #' @description Terminate the relay object.
    #' @return `NULL` (invisibly).
    terminate = function() {
      .condition <<- NULL
      invisible()
    },
    #' @description Set the condition variable to relay from.
    #' @return `NULL` (invisibly).
    #' @param from Condition variable to relay from.
    set_from = function(from) {
      .from <<- from
      invisible()
    },
    #' @description Set the condition variable to relay to.
    #' @return `NULL` (invisibly).
    #' @param to Condition variable to relay to.
    set_to = function(to) {
      .to <<- to
      invisible()
    },
    #' @description Wait until an unobserved task resolves or the timeout
    #'   is reached. Use the throttle to determine the waiting time.
    #' @return `NULL` (invisibly).
    wait = function() {
      timeout <- .subset2(.throttle, "seconds_interval") * 1000
      signal <- nanonext::until_(cv = .condition, msec = timeout)
      .subset2(.throttle, "update")(signal)
      signal
    }
  )
)
