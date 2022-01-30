#' @title Worker class.
#' @export
#' @description `R6` class for a worker.
class_worker <- R6::R6Class(
  classname = "worker",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field name Character of length 1, worker name.
    name = NULL,
    #' @description Worker constructor.
    #' @param name Character of length 1, worker name.
    initialize = function(
      name = basename(tempfile(pattern = "worker_"))
    ) {
      self$name <- name
    },
    #' @description Worker validator.
    validate = function() {
      crew_assert(is.character(self$name) & length(self$name) == 1L)
    }
  )
)
