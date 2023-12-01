#' @title Create a `crew` relay object.
#' @export
#' @family developer
#' @description Create an `R6` `crew` relay object.
#' @details A `crew` relay object keeps the signaling relationships
#'   among condition variables.
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
  private = list(
    #' @field condition Main condition variable.
    .condition = NULL,
    #' @field from Condition variable that `private$.condition` receives
    #'   signals from.
    .from = NULL,
    #' @field to Condition variable that `private$.condition` forwards
    #'   signals to.
    .to = NULL
  ),
  active = list(
    condition = function() {
      .subset2(private, ".condition")
    },
    from = function() {
      .subset2(private, ".from")
    },
    to = function() {
      .subset2(private, ".to")
    }
  ),
  public = list(
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    validate = function() {
      for (field in c(".condition", ".from", ".to")) {
        if (!is.null(private[[field]])) {
          crew_assert(inherits(private[[field]], "conditionVariable"))
        }
      }
      invisible()
    },
    #' @description Start the relay object.
    #' @return `NULL` (invisibly).
    start = function() {
      if (is.null(private$.condition)) {
        private$.condition <- nanonext::cv()
        if (!is.null(private$.from)) {
          nanonext::`%~>%`(cv = private$.from, cv2 = private$.condition)
        }
        if (!is.null(private$.to)) {
          nanonext::`%~>%`(cv = private$.condition, cv2 = private$.to)
        }
      }
      invisible()
    },
    #' @description Terminate the relay object.
    #' @return `NULL` (invisibly).
    terminate = function() {
      private$.condition <- NULL
      invisible()
    },
    #' @description Set the condition variable to relay from.
    #' @return `NULL` (invisibly).
    #' @param from Condition variable to relay from.
    set_from = function(from) {
      private$.from <- from
      invisible()
    },
    #' @description Set the condition variable to relay to.
    #' @return `NULL` (invisibly).
    #' @param to Condition variable to relay to.
    set_to = function(to) {
      private$.to <- to
      invisible()
    },
    #' @description Wait until an unobserved task resolves or the timeout
    #'   is reached.
    #' @return `NULL` (invisibly).
    #' @param seconds_timeout Positive numeric of length 1,
    #'   Number of seconds to wait before timing out.
    wait = function(seconds_timeout = 1e3) {
      timeout <- seconds_timeout * 1000
      condition <- .subset2(self, "condition")
      nanonext::until(cv = condition, msec = timeout)
    }
  )
)
