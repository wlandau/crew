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
    inherit = function(condition) {
      nanonext::`%~>%`(cv = condition, cv2 = self$condition)
      invisible()
    },
    total = function() {
      unobserved <- nanonext::cv_value(.subset2(self, "condition"))
      unobserved + .subset2(self, "unpopped") + .subset2(self, "popped")
    },
    #' @description Decrement the total count of resolved but not
    #'   yet popped tasks.
    #' @details Calls `self$observe()` and then tries to decrement the count.
    #'   Throws an error if the count is already zero.
    #' @return `NULL` (invisibly).
    pop = function() {
      result <- nanonext::until(cv = .subset2(self, "condition"), msec = 0)
      self$unpopped <- .subset2(self, "unpopped") + as.integer(result)
      if (.subset2(self, "unpopped") < 1L) {
        crew_error(
          message = paste(
            "Trying to pop but task resolution count is 0.",
            "Please send a bug report."
          )
        )
      }
      self$unpopped <- .subset2(self, "unpopped") - 1L
      self$popped <- .subset2(self, "popped") + 1L
      invisible()
    },
    #' @description Wait until an unobserved task resolves or the timeout
    #'   is reached.
    wait_unobserved = function(seconds_timeout = 0.25) {
      time <- 1000 * seconds_timeout
      result <- nanonext::until(cv = .subset2(self, "condition"), msec = time)
      self$unpopped <- .subset2(self, "unpopped") + as.integer(result)
      invisible()
    },
    #' @description Wait for periods of `seconds_interval` until at least one
    #'   task arrives.
    wait_unpopped = function(seconds_interval = 0.25) {
      while (.subset2(self, "unpopped")() < 1L) {
        self$wait_unobserved(seconds_timeout = seconds_interval)
      }
    },
    
    wait_total = function(seconds_interval = 0.25, total = 1L) {
      while (.subset2(self, "total")() < total) {
        self$wait_unobserved(seconds_timeout = seconds_interval)
      }
    }
  )
)
