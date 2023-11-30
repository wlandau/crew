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
  public = list(
    #' @field condition Main condition variable.
    condition = NULL,
    #' @field from Condition variable that `self$condition` receives
    #'   signals from.
    from = NULL,
    #' @field to Condition variable that `self$condition` forwards
    #'   signals to.
    to = NULL,
    #' @description Validate the object.
    #' @return `NULL` (invisibly).
    validate = function() {
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
      }
      invisible()
    },
    #' @description Terminate the relay object.
    #' @return `NULL` (invisibly).
    terminate = function() {
      self$condition <- NULL
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
