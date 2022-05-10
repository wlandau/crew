#' @title Abstract store
#' @export
#' @aliases crew_store
#' @family store
#' @description Abstract class for a data store.
#' @details Most `crew` queues use data stores to send and receive task data.
#'   Some use entirely local data, others may use cloud data to support
#'   cloud workers. Stores have a "main" file space and a "worker" file space.
#'   The main file space is usually on the local machine, and the worker
#'   file space may be in the cloud. Sending and collecting data may involve
#'   shuffling files to and from the main file space and the worker file space.
#' @examples
#' # Usage on its own:
#' store <- crew_store_local$new(timeout = Inf)
#' store$write_main_input("worker", list(value = "x"))
#' store$list_main_input()
#' store$exists_main_input("worker")
#' store$read_main_input("worker")
#' # Usage in a queue:
#' fun <- function(x) x + 1
#' args <- list(x = 1)
#' queue <- crew_queue_future$new(timeout = 60, store = store)
#' queue$push(fun = fun, args = args)
#' queue$block()
#' result <- queue$pop()
#' str(result)
#' result$result$result
#' queue$shutdown()
#' processx::supervisor_kill()
#' store$destroy()
crew_store <- R6::R6Class(
  classname = "crew_store",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    dir_root = NULL,
    timeout = NULL,
    wait = NULL,
    read_local = function(dir, worker) {
      crew_true(dir, is.character(.), !anyNA(.), length(.) == 1L)
      path <- file.path(private$dir_root, dir, worker)
      crew_true(
        path,
        is.character(.),
        !anyNA(.),
        length(.) == 1L,
        all(file.exists(.))
      )
      qs::qread(file = path)
    },
    write_local = function(dir, worker, value) {
      crew_true(dir, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(worker, is.character(.), !anyNA(.), length(.) == 1L)
      path_temp <- file.path(private$dir_root, "temp", crew_name())
      path <- file.path(private$dir_root, dir, worker)
      dir_create(dirname(path_temp))
      dir_create(dirname(path))
      qs::qsave(x = value, file = path_temp)
      crew_wait(
        fun = function(path_temp) file.exists(path_temp),
        args = list(path_temp = path_temp),
        timeout = private$timeout,
        wait = private$wait,
        message = "store timeout writing local file"
      )
      crew_wait(
        fun = function(path, path_temp) file.rename(from = path_temp, to = path),
        args = list(path = path, path_temp = path_temp),
        timeout = private$timeout,
        wait = private$wait,
        message = "store timeout moving local file after writing"
      )
      invisible()
    },
    exists_local = function(dir, worker) {
      crew_true(dir, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(worker, is.character(.), !anyNA(.), length(.) == 1L)
      all(file.exists(file.path(private$dir_root, dir, worker)))
    },
    list_local = function(dir) {
      crew_true(dir, is.character(.), !anyNA(.), length(.) == 1L)
      list.files(file.path(private$dir_root, dir))
    },
    delete_local = function(dir, worker) {
      crew_true(dir, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(worker, is.character(.), !anyNA(.), length(.) == 1L)
      path <- file.path(private$dir_root, dir, worker)
      path_temp <- file.path(private$dir_root, "temp", crew_name())
      crew_wait(
        fun = function(path, path_temp) file.rename(from = path, to = path_temp),
        args = list(path = path, path_temp = path_temp),
        timeout = private$timeout,
        wait = private$wait,
        message = "store timeout moving local file for deletion"
      )
      unlink(path_temp, recursive = TRUE, force = TRUE)
      invisible()
    }
  ),
  public = list(
    #' @description Abstract store constructor.
    #' @return An abstract store object.
    #' @param dir_root Character of length 1, root directory of the data store
    #'   on the local file system.
    #' @param timeout Numeric of length 1, number of seconds to wait
    #'   to receive worker input or shuffle files in the worker loop.
    #' @param wait Numeric of length 1, number of seconds to pause between
    #'   iterations waiting for files to arrive in their proper places
    #'   in internal operations.
    initialize = function(
      dir_root = tempfile(),
      timeout = 60,
      wait = 1
    ) {
      crew_true(dir_root, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(timeout, is.numeric(.), length(.) == 1, . >= 0, !anyNA(.))
      crew_true(wait, is.numeric(.), length(.) == 1, . >= 0, !anyNA(.))
      private$dir_root <- dir_root
      private$timeout <- timeout
      private$wait <- wait
    },
    #' @description Get the root directory of the data store.
    #' @return The root directory of the data store.
    get_root = function() {
      private$dir_root
    },
    #' @description Read worker input data from the main file space.
    #' @return Worker input data from the main file space.
    #' @param worker Character of length 1, name of the worker.
    read_main_input = function(worker) {
      private$read_local(dir = "main_input", worker = worker)
    },
    #' @description Read worker output data from the main file space.
    #' @return Worker output data from the main file space.
    #' @param worker Character of length 1, name of the worker.
    read_main_output = function(worker) {
      private$read_local(dir = "main_output", worker = worker)
    },
    #' @description Write worker input data to the main file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    #' @param value R object to write.
    write_main_input = function(worker, value) {
      private$write_local(dir = "main_input", worker = worker, value = value)
    },
    #' @description Write worker output data to the main file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    #' @param value R object to output.
    write_main_output = function(worker, value) {
      private$write_local(dir = "main_output", worker = worker, value = value)
    },
    #' @description Check if worker input data exists in the main file space.
    #' @return Logical of length 1, whether worker input data exists
    #'   in the main file space.
    #' @param worker Character of length 1, name of the worker.
    exists_main_input = function(worker) {
      private$exists_local(dir = "main_input", worker = worker)
    },
    #' @description Check if worker output data exists in the main file space.
    #' @return Logical of length 1, whether worker output data exists
    #'   in the main file space.
    #' @param worker Character of length 1, name of the worker.
    exists_main_output = function(worker) {
      private$exists_local(dir = "main_output", worker = worker)
    },
    #' @description List workers with input data in the main file space.
    #' @return Character vector of workers with input data
    #'   in the main file space.
    list_main_input = function() {
      private$list_local(dir = "main_input")
    },
    #' @description List workers with output data in the main file space.
    #' @return Character vector of workers with output data
    #'   in the main file space.
    list_main_output = function() {
      private$list_local(dir = "main_output")
    },
    #' @description Delete worker input data from the main file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    delete_main_input = function(worker) {
      private$delete_local(dir = "main_input", worker = worker)
    },
    #' @description Delete worker output data from the main file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    delete_main_output = function(worker) {
      private$delete_local(dir = "main_output", worker = worker)
    },
    #' @description Delete the files of the data store.
    #' @details The local files, along with the main file space,
    #'   are always deleted. Other deletions may depend on the
    #'   specific store subclass.
    #' @return `NULL` (invisibly).
    destroy = function() {
      unlink(private$dir_root, recursive = TRUE, force = TRUE)
    }
  )
)
