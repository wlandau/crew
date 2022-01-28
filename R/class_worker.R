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
    #' @field data Named list of user-defined worker data.
    data = NULL,
    #' @param name Character of length 1, worker name.
    #' @param data Named list of user-defined worker data.
    initialize = function(
      name = basename(tempfile(pattern = "worker_")),
      data = list()
    ) {
      self$name <- name
      self$data <- data
    },
    #' @description Worker validator.
    validate = function() {
      crew_assert(is.character(self$name) & length(self$name) == 1L)
      crew_assert(is.list(self$data))
      crew_assert(length(names(self$data)) == length(self$data))
    }
  )
)
