#' @title Local store class.
#' @export
#' @aliases store_local
#' @description `R6` class for a local store.
class_store_local <- R6::R6Class(
  classname = "store_local",
  inherit = crew::class_store,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    path_direction = function(name, direction) {
      if_any(
        identical(direction, "input"),
        self$path_input(name),
        self$path_output(name)
      )
    },
    read = function(name, direction) {
      crew_assert_chr_scalar(name)
      path <- private$path_direction(name, direction)
      msg <- paste(
        "input file",
        path,
        "of worker",
        name,
        "does not exist."
      )
      if (!all(file.exists(path))) {
        crew_error(msg)
      }
      qs::qread(file = path)
    },
    write = function(name, data, direction) {
      crew_assert_chr_scalar(name)
      path_temp <- self$path_temp(name)
      path <- private$path_direction(name, direction)
      fs::dir_create(dirname(path_temp))
      fs::dir_create(dirname(path))
      qs::qsave(x = data, file = path_temp)
      file.rename(from = path_temp, to = path)
      invisible()
    },
    exists = function(name, direction) {
      crew_assert_chr_scalar(name)
      all(file.exists(private$path_direction(name, direction)))
    },
    delete = function(name, direction) {
      crew_assert_chr_scalar(name)
      path <- private$path_direction(name, direction)
      unlink(path, recursive = TRUE, force = TRUE)
      invisible()
    }
  ),
  public = list(
    #' @field dir_temp Character of length 1, directory for temporary files.
    dir_temp = NULL,
    #' @description Store constructor.
    #' @param dir_root Character of length 1, file path or prefix
    #'   where all the files are located.
    initialize = function(
      dir_root = tempfile()
    ) {
      super$initialize(dir_root = dir_root)
      self$dir_temp <- file.path(self$dir_root, "temp")
    },
    #' @description Path to a worker's temporary file.
    #' @param name Worker name.
    path_temp = function(name) {
      crew_assert_chr_scalar(name)
      file.path(self$dir_temp, name)
    },
    #' @description Read worker input.
    #' @param name Character of length 1, Worker name.
    read_input = function(name) {
      private$read(name = name, direction = "input")
    },
    #' @description Read worker output.
    #' @param name Character of length 1, Worker name.
    read_output = function(name) {
      private$read(name = name, direction = "output")
    },
    #' @description Write worker input.
    #' @param name Character of length 1, Worker name.
    #' @param data Data to write.
    write_input = function(name, data) {
      private$write(
        name = name,
        data = data,
        direction = "input"
      )
    },
    #' @description Write worker output.
    #' @param name Character of length 1, Worker name.
    #' @param data Data to write.
    write_output = function(name, data) {
      private$write(
        name = name,
        data = data,
        direction = "output"
      )
    },
    #' @description Exists worker input?
    #' @param name Character of length 1, Worker name.
    exists_input = function(name) {
      private$exists(name = name, direction = "input")
    },
    #' @description Exists worker output?
    #' @param name Character of length 1, Worker name.
    exists_output = function(name) {
      private$exists(name = name, direction = "output")
    },
    #' @description Delete worker input.
    #' @param name Character of length 1, Worker name
    delete_input = function(name) {
      private$delete(name = name, direction = "input")
    },
    #' @description Delete worker output.
    #' @param name Character of length 1, Worker name
    delete_output = function(name) {
      private$delete(name = name, direction = "output")
    },
    #' @description Delete all the files in the data store.
    #' @param name Character of length 1, Worker name
    destroy = function() {
      unlink(self$dir_root, recursive = TRUE, force = TRUE)
      invisible()
    }
  )
)
