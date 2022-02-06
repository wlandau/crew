#' @title Store class.
#' @export
#' @aliases store
#' @description `R6` class for a data store.
#' @details A data store is how a worker object sends jobs to its
#'   underlying R process and receives output back. Subclasses may
#'   use the local file system or cloud storage (e.g. Amazon S3 buckets).
#'   A store object is part of a crew.
#' @examples
#' store <- class_store_local$new()
#' store$write_output("worker_name", list(value = "job_output"))
#' store$read_output("worker_name")
class_store <- R6::R6Class(
  classname = "store",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field dir_root Character of length 1, root file path or prefix
    #'   where the files are located.
    dir_root = NULL,
    #' @field dir_input Character of length 1, worker input directory.
    dir_input = NULL,
    #' @field dir_output Character of length 1, worker output directory.
    dir_output = NULL,
    #' @description Store constructor.
    #' @return The `new()` method calls the constructor and returns
    #'   a new store object.
    #' @param dir_root Character of length 1, file path or prefix
    #'   where all the files are located.
    initialize = function(
      dir_root = tempfile()
    ) {
      crew_assert_chr_scalar(dir_root)
      self$dir_root <- dir_root
      self$dir_input <- file.path(self$dir_root, "input")
      self$dir_output <- file.path(self$dir_root, "output")
    },
    #' @description Path to job input.
    #' @return Character of length 1, path to job input for a given worker.
    #' @param name Worker name.
    path_input = function(name) {
      crew_assert_chr_scalar(name)
      file.path(self$dir_input, name)
    },
    #' @description Path to job output.
    #' @return Character of length 1, path to job output for a given worker.
    #' @param name Worker name.
    path_output = function(name) {
      crew_assert_chr_scalar(name)
      file.path(self$dir_output, name)
    },
    #' @description Store validator.
    #' @return `NULL` (invisibly).
    validate = function() {
      for (field in c("dir_root", "dir_input", "dir_output")) {
        value <- self[[field]]
        crew_assert_chr_scalar(value, paste("store: invalid ", field))
      }
      funs <- c(
        "path_input",
        "path_output",
        "read_input",
        "read_output",
        "write_input",
        "write_output",
        "exists_input",
        "exists_output",
        "delete_input",
        "delete_output",
        "destroy",
        "marshal",
        "validate"
      )
      for (fun in funs) {
        crew_assert(is.function(self[[fun]]), paste(fun, "method undefined"))
      }
    }
  )
)
