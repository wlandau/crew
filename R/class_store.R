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
    #' @field dir_workers Character of length 1, worker storage directory.
    dir_workers = NULL,
    #' @field dir_temp Character of length 1, directory for temporary files.
    dir_temp = NULL,
    #' @description Store constructor.
    #' @param dir Character of length 1, file path or prefix where the files
    #'   are located.
    initialize = function(
      dir = tempfile()
    ) {
      self$dir <- dir
      self$dir_workers <- file.path(self$dir, "workers")
      self$dir_temp <- file.path(self$root, "temp")
    },
    #' @description Store validator.
    validate = function() {
      for (field in c("dir", "dir_workers", "dir_temp")) {
        dir <- self[[field]]
        crew_assert(
          is.character(dir) & length(dir) == 1L & nzchar(dir),
          paste("store has an invalid", field, "directory.")
        )
      }
    }
  )
)
