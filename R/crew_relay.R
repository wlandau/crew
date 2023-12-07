#' @title Create a `crew` relay object.
#' @export
#' @family relay
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
#' @family relay
#' @description `R6` class for relay configuration.
#' @details See [crew_relay()].
#' @examples
#' crew_relay()
crew_class_relay <- R6::R6Class(
  classname = "crew_class_relay",
  cloneable = FALSE,
  private = list(
    .condition = NULL,
    .from = NULL,
    .to = NULL
  ),
  active = list(
    #' @field condition Main condition variable.
    condition = function() {
      .subset2(private, ".condition")
    },
    #' @field from Condition variable to relay from.
    from = function() {
      .subset2(private, ".from")
    },
    #' @field to Condition variable to relay to.
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
      nanonext::until_(cv = condition, msec = timeout)
    }
  )
)
