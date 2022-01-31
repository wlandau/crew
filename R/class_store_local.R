#' @title Local store class.
#' @export
#' @aliases store
#' @description `R6` class for a local store.
class_store_local <- R6::R6Class(
  classname = "store_local",
  inherit = crew::class_store,
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field dir_temp Character of length 1, directory for temporary files.
    dir_temp = NULL,
    #' @description Store constructor.
    #' @param dir_root Character of length 1, file path or prefix where the files
    #'   are located.
    initialize = function(
      dir_root = tempfile()
    ) {
      super$initialize(dir_root = dir_root)
      self$dir_temp <- file.path(self$dir_root, "temp")
    },
    #' @description Path to a worker's temporary file.
    #' @param name Worker name.
    path_temp = function(name) {
      crew_assert(is.character(name) & length(name) == 1L & nzchar(name))
      file.path(self$dir_temp, name)
    },
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
