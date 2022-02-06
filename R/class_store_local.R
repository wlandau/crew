#' @title Local store class.
#' @export
#' @aliases store_local
#' @description `R6` class for a local store. Worker files are on disk
#'   on the same machine that manages the crew.
#' @examples
#' store <- class_store_local$new()
#' store$write_output("worker_name", list(value = "job_output"))
#' store$read_output("worker_name")
class_store_local <- R6::R6Class(
  classname = "store_local",
  inherit = crew::class_store,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    path_direction = function(name, direction) {
      crew_assert_chr_scalar(name)
      crew_assert(direction %in% c("input", "output"))
      out <- if_any(
        identical(direction, "input"),
        self$path_input(name),
        self$path_output(name)
      )
      out
    },
    read = function(name, direction) {
      crew_assert_chr_scalar(name)
      crew_assert(direction %in% c("input", "output"))
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
    write = function(name, data, direction, timeout, wait) {
      crew_assert_chr_scalar(name)
      crew_assert(direction %in% c("input", "output"))
      path_temp <- self$path_temp(name)
      path <- private$path_direction(name, direction)
      dir_create(dirname(path_temp))
      dir_create(dirname(path))
      qs::qsave(x = data, file = path_temp)
      crew_wait(
        fun = function(path_temp) file.exists(path_temp),
        args = list(path_temp = path_temp),
        timeout = timeout,
        wait = wait
      )
      file.rename(from = path_temp, to = path)
      crew_wait(
        fun = function(path, path_temp) {
          file.exists(path) && !file.exists(path_temp)
        },
        args = list(path = path, path_temp = path_temp),
        timeout = timeout,
        wait = wait
      )
      invisible()
    },
    exists = function(name, direction) {
      crew_assert_chr_scalar(name)
      crew_assert(direction %in% c("input", "output"))
      all(file.exists(private$path_direction(name, direction)))
    },
    delete = function(name, timeout, wait, direction) {
      crew_assert_chr_scalar(name)
      crew_assert(direction %in% c("input", "output"))
      path <- private$path_direction(name, direction)
      unlink(path, recursive = TRUE, force = TRUE)
      crew_wait(
        fun = function(path) !file.exists(path),
        args = list(path = path),
        timeout = timeout,
        wait = wait
      )
      invisible()
    }
  ),
  public = list(
    #' @field dir_temp Character of length 1, directory for temporary files.
    dir_temp = NULL,
    #' @description Store constructor.
    #' @return The `new()` method calls the constructor and returns a new
    #'   data store object.
    #' @param dir_root Character of length 1, file path or prefix
    #'   where all the files are located.
    initialize = function(
      dir_root = tempfile()
    ) {
      super$initialize(dir_root = dir_root)
      self$dir_temp <- file.path(self$dir_root, "temp")
    },
    #' @description Path to a worker's temporary file.
    #' @details Temporary files are staging areas for writing large data.
    #'   In the write methods, the file is written to the temporary file
    #'   and then moved over to the permanent location. This safeguards
    #'   against corrupted persistent files that may result if
    #'   the file is not finished writing and R crashes.
    #' @return Character of length 1, path to the worker's temporary file.
    #' @param name Worker name.
    path_temp = function(name) {
      crew_assert_chr_scalar(name)
      base <- basename(tempfile(pattern = paste0(name, "_")))
      file.path(self$dir_temp, base)
    },
    #' @description Read worker input.
    #' @return Input data sent to a worker. Should contain a job
    #'   and required data to run it.
    #' @param name Character of length 1, Worker name.
    read_input = function(name) {
      private$read(name = name, direction = "input")
    },
    #' @description Read worker output.
    #' @return Output data returned from a worker. Should contain
    #'   the result of a job.
    #' @param name Character of length 1, Worker name.
    read_output = function(name) {
      private$read(name = name, direction = "output")
    },
    #' @description Write worker input.
    #' @return `NULL` (invisibly).
    #' @param name Character of length 1, Worker name.
    #' @param data Data to write. Should contain a job and the required
    #'   data to run it.
    #' @param timeout Positive numeric of length 1, number of seconds to wait
    #'   for a successful write.
    #' @param wait Number of seconds to wait between
    #'   checks that the file exists.
    write_input = function(name, data, timeout = 60, wait = 1) {
      private$write(
        name = name,
        data = data,
        timeout = timeout,
        wait = wait,
        direction = "input"
      )
    },
    #' @description Write worker output.
    #' @return `NULL` (invisibly).
    #' @param name Character of length 1, Worker name.
    #' @param data Data to write. Should contain the result of a job.
    #' @param timeout Positive numeric of length 1, number of seconds to wait
    #'   for a successful write.
    #' @param wait Number of seconds to wait between
    #'   checks that the file exists.
    write_output = function(name, data, timeout = 60, wait = 1) {
      private$write(
        name = name,
        data = data,
        timeout = timeout,
        wait = wait,
        direction = "output"
      )
    },
    #' @description Check if worker input exists.
    #' @return `TRUE` if worker input exists and `FALSE` otherwise.
    #' @param name Character of length 1, Worker name.
    exists_input = function(name) {
      private$exists(name = name, direction = "input")
    },
    #' @description Check if worker output exists.
    #' @return `TRUE` if worker input exists and `FALSE` otherwise.
    #' @param name Character of length 1, Worker name.
    exists_output = function(name) {
      private$exists(name = name, direction = "output")
    },
    #' @description Delete worker input.
    #' @return `NULL` (invisibly).
    #' @param name Character of length 1, worker name.
    #' @param timeout Positive numeric of length 1, number of seconds to wait
    #'   for a successful deletion.
    #' @param wait Number of seconds to wait between
    #'   checks that the file is gone.
    delete_input = function(name, timeout = 60, wait = 1) {
      private$delete(
        name = name,
        timeout = timeout,
        wait = wait,
        direction = "input"
      )
    },
    #' @description Delete worker output.
    #' @return `NULL` (invisibly).
    #' @param name Character of length 1, worker name.
    #' @param timeout Positive numeric of length 1, number of seconds to wait
    #'   for a successful deletion.
    #' @param wait Number of seconds to wait between
    #'   checks that the file is gone.
    delete_output = function(name, timeout = 60, wait = 1) {
      private$delete(
        name = name,
        timeout = timeout,
        wait = wait,
        direction = "output"
      )
    },
    #' @description Delete all the files in the data store.
    #' @return `NULL` (invisibly).
    #' @param name Character of length 1, Worker name
    destroy = function() {
      unlink(self$dir_root, recursive = TRUE, force = TRUE)
      invisible()
    },
    #' @description Marshal the `R6` store object to ship to a worker.
    #' @return Character of length 1 with R code to run to reconstruct
    #'   the store on a worker.
    marshal = function() {
      expr <- substitute(
        crew::class_store_local$new(dir_root = dir_root),
        list(dir_root = self$dir_root)
      )
      paste(deparse(expr), collapse = "\n")
    }
  )
)
