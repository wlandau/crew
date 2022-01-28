#' @title Crew class.
#' @export
#' @description `R6` class for a crew..
class_crew <- R6::R6Class(
  classname = "crew",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field name Character of length 1, crew name.
    name = NULL,
    #' @field data Named list of user-defined crew data.
    data = NULL,
    #' @field workers Named list of `R6` worker objects.
    workers = NULL,
    #' @description Crew constructor.
    #' @param name Character of length 1, crew name.
    #' @param workers Named list of `R6` worker objects.
    #' @param data Named list of user-defined crew data.
    initialize = function(
      name =  basename(tempfile(pattern = "crew_")),
      data = list(),
      workers = list()
    ) {
      self$name <- name
      self$data <- data
      self$workers <- workers
    },
    #' @description Crew validator.
    validate = function() {
      crew_assert(is.character(self$name) & length(self$name) == 1L)
      crew_assert(is.list(self$data))
      crew_assert(length(names(self$data)) == length(self$data))
      crew_assert(is.list(self$workers))
      crew_assert(length(names(self$workers)) == length(self$workers))
      lapply(self$workers, function(x) crew_assert(inherits(x, "worker")))
      lapply(self$workers, function(x) x$validate())
    }
  )
)
