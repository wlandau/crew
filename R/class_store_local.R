#' @title Local store class.
#' @export
#' @description `R6` class for a local store.
class_store_local <- R6::R6Class(
  classname = "store_local",
  inherit = class_store,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @description Write worker data.
    #' @param name Character of length 1, worker name.
    #' @param data Data to write.
    write = function(name, data) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      dir_create(self$dir_temp)
      dir_create(self$dir_workers)
      path_temp <- file.path(self$dir_temp, name)
      path <- file.path(self$dir_workers, name)
      qs::qsave(x = output, file = path_temp)
      file.rename(from = path_temp, to = path)
      invisible()
    },
    #' @description Read worker output.
    #' @param name Character of length 1, worker name.
    #' @param output Worker output to write.
    read = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      path <- file.path(self$dir_workers, name)
      msg <- paste("input file", path, "of worker", name, "does not exist.")
      if (!all(file.exists(path))) {
        crew_error(msg)
      }
      qs::qread(file = path)
    },
    #' @description Exists worker data?
    #' @param name Character of length 1, worker name
    exists = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      all(file.exists(file.path(self$dir_workers, name)))
    },
    #' @description Delete worker data.
    #' @param name Character of length 1, worker name
    delete = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      path <- file.path(self$dir_workers, name)
      unlink(path, recursive = TRUE, force = TRUE)
      invisible()
    },
    #' @description Delete all the files in the data store.
    #' @param name Character of length 1, worker name
    destroy = function() {
      unlink(self$dir, recursive = TRUE, force = TRUE)
      invisible()
    }
  )
)
