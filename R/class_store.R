#' @title Store class.
#' @export
#' @description `R6` class for a store.
class_store <- R6::R6Class(
  classname = "store",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field dir_root Character of length 1, root file path or prefix
    #'   where the files are located.
    dir_root = NULL,
    #' @field dir Character of length 1, worker input directory.
    dir_input = NULL,
    #' @field dir Character of length 1, worker output directory.
    dir_output = NULL,

    #' @description Store constructor.
    #' @param dir_root Character of length 1, file path or prefix where the files
    #'   are located.
    initialize = function(
      dir_root = tempfile()
    ) {
      self$dir_root <- dir_root
      self$dir_input <- file.path(self$dir_root, "input")
      self$dir_output <- file.path(self$dir_root, "output")
    },
    #' @description Path to job input.
    #' @param job_name Job name.
    path_input = function(job_name) {
      crew_assert(is.character(job_name) & length(job_name) == 1L & nzchar(job_name))
      file.path(self$dir_input, job_name)
    },
    #' @description Path to job input.
    #' @param job_name Job name.
    path_output = function(job_name) {
      crew_assert(is.character(job_name) & length(job_name) == 1L & nzchar(job_name))
      file.path(self$dir_output, job_name)
    },
    #' @description Store validator.
    validate = function() {
      for (field in c("dir_root", "dir_input", "dir_output")) {
        value <- self[[field]]
        crew_assert(
          is.character(value) & length(value) == 1L & nzchar(value),
          paste("store has an invalid", field, "directory.")
        )
      }
    }
  )
)
