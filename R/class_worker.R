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
    #' @field crew `R6` crew object to which the worker belongs.
    crew = NULL,
    #' @description Worker constructor.
    #' @param name Character of length 1, worker name.
    #' @param crew `R6` crew object to which the worker belongs.
    initialize = function(
      name = basename(tempfile(pattern = "worker_")),
      crew = NULL
    ) {
      self$name <- name
      self$crew <- crew
    },
    #' @description Worker validator.
    validate = function() {
      crew_assert_chr_scalar(self$name, "worker has invalid name.")
      crew_assert(
        inherits(self$crew, "crew"),
        paste("invalid crew object in worker", self$name)
      )
      crew_assert(
        identical(self$crew$worker_list[[self$name]], self),
        paste(
          "worker",
          self$name,
          "falsely claims it is part of crew",
          self$crew$name
        )
      )
    }
  )
)
