#' @title Store class.
#' @export
#' @description `R6` class for a store.
class_store <- R6::R6Class(
  classname = "store",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field root Character of length 1, root file path or prefix
    #'   where the files are located.
    root = NULL,
    #' @field dir Character of length 1, worker persistent storage directory.
    dir = NULL,
    #' @field dir_temp Character of length 1, directory for temporary files.
    dir_temp = NULL,
    #' @description Store constructor.
    #' @param root Character of length 1, file path or prefix where the files
    #'   are located.
    initialize = function(
      root = tempfile()
    ) {
      self$root <- root
      self$dir <- file.path(self$root, "worker")
      self$dir_temp <- file.path(self$root, "temp")
    },
    #' @description Path to a worker's persistent file.
    #' @param name Worker name.
    path = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      file.path(self$dir, name)
    },
    #' @description Path to a worker's temporary file.
    #' @param name Worker name.
    path_temp = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      file.path(self$dir_temp, name)
    },
    #' @description Store validator.
    validate = function() {
      for (field in c("root", "dir", "dir_temp")) {
        value <- self[[field]]
        crew_assert(
          is.character(value) & length(value) == 1L & nzchar(value),
          paste("store has an invalid", field, "directory.")
        )
      }
    }
  )
)
