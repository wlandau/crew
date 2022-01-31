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
      path_temp <- self$path_temp(name)
      path <- self$path(name)
      fs::dir_create(dirname(path_temp))
      fs::dir_create(dirname(path))
      qs::qsave(x = data, file = path_temp)
      file.rename(from = path_temp, to = path)
      invisible()
    },
    #' @description Read worker output.
    #' @param name Character of length 1, worker name.
    #' @param output Worker output to write.
    read = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      path <- self$path(name)
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
      all(file.exists(self$path(name)))
    },
    #' @description Delete worker data.
    #' @param name Character of length 1, worker name
    delete = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      path <- self$path(name)
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
