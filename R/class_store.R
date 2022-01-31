#' @title Store class.
#' @export
#' @description `R6` class for a store.
class_store <- R6::R6Class(
  classname = "store",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field dir Character of length 1, root file path or prefix
    #'   where the files are located.
    dir = NULL,
    #' @field dir_input Character of length 1, worker input directory.
    dir_input = NULL,
    #' @field dir_input Character of length 1, worker output directory.
    dir_output = NULL,
    #' @field dir_temp Character of length 1, directory for temporary files.
    dir_temp = NULL,
    #' @description Store constructor.
    #' @param dir Character of length 1, file path or prefix where the files
    #'   are located.
    initialize = function(
      dir = tempfile()
    ) {
      self$dir <- dir
      self$dir_input <- file.path(self$dir, "input")
      self$dir_output <- file.path(self$dir, "output")
      self$dir_temp <- file.path(self$root, "temp")
    }
    #' @description Store validator.
    validate = function() {
      crew_assert(
        is.character(self$dir) & length(self$dir) == 1L & nzchar(self$dir),
        "store has an invalid root directory."
      )
    }
  )
)
