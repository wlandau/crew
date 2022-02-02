#' @title Local store class.
#' @export
#' @aliases store
#' @description `R6` class for a local store.
class_store_local <- R6::R6Class(
  classname = "store_local",
  inherit = crew::class_store,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    path_direction = function(worker_name, direction) {
      if_any(
        identical(direction, "input"),
        self$path_input(worker_name),
        self$path_output(worker_name)
      )
    },
    write = function(worker_name, data, direction) {
      crew_assert_chr_scalar(worker_name)
      path_temp <- self$path_temp(worker_name)
      path <- private$path_direction(worker_name, direction)
      fs::dir_create(dirname(path_temp))
      fs::dir_create(dirname(path))
      qs::qsave(x = data, file = path_temp)
      file.rename(from = path_temp, to = path)
      invisible()
    },
    read = function(worker_name, direction) {
      crew_assert_chr_scalar(worker_name)
      path <- private$path_direction(worker_name, direction)
      msg <- paste(
        "input file",
        path,
        "of worker",
        worker_name,
        "does not exist."
      )
      if (!all(file.exists(path))) {
        crew_error(msg)
      }
      qs::qread(file = path)
    },
    exists = function(worker_name, direction) {
      crew_assert_chr_scalar(worker_name)
      all(file.exists(private$path_direction(worker_name, direction)))
    },
    delete = function(worker_name, direction) {
      crew_assert_chr_scalar(worker_name)
      path <- private$path_direction(worker_name, direction)
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
    #' @param worker_name Worker name.
    path_temp = function(worker_name) {
      crew_assert_chr_scalar(worker_name)
      file.path(self$dir_temp, worker_name)
    },
    #' @description Write worker input.
    #' @param worker_name Character of length 1, Worker name.
    #' @param data Data to write.
    write_input = function(worker_name, data) {
      private$write(
        worker_name = worker_name,
        data = data,
        direction = "input"
      )
    },
    #' @description Write worker output.
    #' @param worker_name Character of length 1, Worker name.
    #' @param data Data to write.
    write_output = function(worker_name, data) {
      private$write(
        worker_name = worker_name,
        data = data,
        direction = "output"
      )
    },
    #' @description Read worker input.
    #' @param worker_name Character of length 1, Worker name.
    read_input = function(worker_name) {
      private$read(worker_name = worker_name, direction = "input")
    },
    #' @description Read worker output.
    #' @param worker_name Character of length 1, Worker name.
    read_output = function(worker_name) {
      private$read(worker_name = worker_name, direction = "output")
    },
    #' @description Exists worker input?
    #' @param worker_name Character of length 1, Worker name.
    exists_input = function(worker_name) {
      private$exists(worker_name = worker_name, direction = "input")
    },
    #' @description Exists worker output?
    #' @param worker_name Character of length 1, Worker name.
    exists_output = function(worker_name) {
      private$exists(worker_name = worker_name, direction = "output")
    },
    #' @description Delete worker input.
    #' @param worker_name Character of length 1, Worker name
    delete_input = function(worker_name) {
      private$delete(worker_name = worker_name, direction = "input")
    },
    #' @description Delete worker output.
    #' @param worker_name Character of length 1, Worker name
    delete_output = function(worker_name) {
      private$delete(worker_name = worker_name, direction = "output")
    },
    #' @description Delete all the files in the data store.
    #' @param worker_name Character of length 1, Worker name
    destroy = function() {
      unlink(self$dir_root, recursive = TRUE, force = TRUE)
      invisible()
    }
  )
)
