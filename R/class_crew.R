#' @title Crew class.
#' @export
#' @description `R6` class for a crew.
class_crew <- R6::R6Class(
  classname = "crew",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field name Character of length 1, crew name.
    name = NULL,
    #' @field worker_class `R6ClassGenerator`
    #'   object with the worker definition.
    worker_class = NULL,
    #' @field workers Named list of worker objects.
    workers = NULL,
    #' @description Crew constructor.
    #' @param name Character of length 1, crew name.
    #' @param worker_class `R6ClassGenerator`
    #'   object with the worker definition.
    #' @param workers Named list of `R6` worker objects.
    initialize = function(
      name =  basename(tempfile(pattern = "crew_")),
      worker_class = class_worker,
      workers = list()
    ) {
      self$name <- name
      self$worker_class <- worker_class
      self$workers <- workers
    },
    #' @description Crew validator.
    validate = function() {
      crew_assert(is.character(self$name) & length(self$name) == 1L)
      crew_assert(inherits(self$worker_class, "R6ClassGenerator"))
      crew_assert(is.list(self$workers))
      crew_assert(length(names(self$workers)) == length(self$workers))
      lapply(self$workers, function(x) crew_assert(inherits(x, "worker")))
      lapply(self$workers, function(x) x$validate())
    }
  )
)
