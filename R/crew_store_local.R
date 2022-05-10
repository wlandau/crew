#' @title Local store
#' @export
#' @aliases crew_store_local
#' @family store
#' @description Local store class.
#' @details Most `crew` queues use data stores to send and receive task data.
#'   Some use entirely local data, others may use cloud data to support
#'   cloud workers. The local store works with files on the local file system.
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
crew_store_local <- R6::R6Class(
  classname = "crew_store_local",
  inherit = crew_store,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    send_file = function(from, to) {
      crew_true(
        from,
        is.character(.),
        !anyNA(.),
        length(.) == 1L,
        all(file.exists(.))
      )
      crew_true(to, is.character(.), !anyNA(.), length(.) == 1L)
      dir_create(dirname(to))
      crew_wait(
        fun = function(from, to) file.rename(from = from, to = to),
        args = list(from = from, to = to),
        timeout = private$timeout,
        wait = private$wait,
        "local store timeout sending file"
      )
      invisible()
    }
  ),
  public = list(
    #' @description Read worker input data from the worker file space.
    #' @return Worker input data from the worker file space.
    #' @param worker Character of length 1, name of the worker.
    read_worker_input = function(worker) {
      private$read_local(dir = "worker_input", worker = worker)
    },
    #' @description Read worker output data from the worker file space.
    #' @return Worker output data from the worker file space.
    #' @param worker Character of length 1, name of the worker.
    read_worker_output = function(worker) {
      private$read_local(dir = "worker_output", worker = worker)
    },
    #' @description Write worker input data to the worker file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    #' @param value R object to write.
    write_worker_input = function(worker, value) {
      private$write_local(dir = "worker_input", worker = worker, value = value)
    },
    #' @description Write worker output data to the worker file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    #' @param value R object to output.
    write_worker_output = function(worker, value) {
      private$write_local(dir = "worker_output", worker = worker, value = value)
    },
    #' @description Check if worker input data exists in the worker file space.
    #' @return Logical of length 1, whether worker input data exists
    #'   in the worker file space.
    #' @param worker Character of length 1, name of the worker.
    exists_worker_input = function(worker) {
      private$exists_local(dir = "worker_input", worker = worker)
    },
    #' @description Check if worker output data exists in the worker file space.
    #' @return Logical of length 1, whether worker output data exists
    #'   in the worker file space.
    #' @param worker Character of length 1, name of the worker.
    exists_worker_output = function(worker) {
      private$exists_local(dir = "worker_output", worker = worker)
    },
    #' @description List workers with input data in the worker file space.
    #' @return Character vector of workers with input data
    #'   in the worker file space.
    list_worker_input = function() {
      private$list_local(dir = "worker_input")
    },
    #' @description List workers with output data in the worker file space.
    #' @return Character vector of workers with output data
    #'   in the worker file space.
    list_worker_output = function() {
      private$list_local(dir = "worker_output")
    },
    #' @description Delete worker input data from the worker file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    delete_worker_input = function(worker) {
      private$delete_local(dir = "worker_input", worker = worker)
    },
    #' @description Delete worker output data from the worker file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    delete_worker_output = function(worker) {
      private$delete_local(dir = "worker_output", worker = worker)
    },
    #' @description Upload worker input data from the main file space
    #'   to the worker file space.
    #' @details For the local store, this is not a true upload. It is
    #'   just moving a file.
    #' @return `NULL` (invisibly)
    #' @param worker Character of length 1, name of the worker.
    upload_input = function(worker) {
      from <- file.path(private$dir_root, "main_input", worker)
      to <- file.path(private$dir_root, "worker_input", worker)
      private$send_file(from = from, to = to)
    },
    #' @description Download worker input data from the worker file space
    #'   to the main file space.
    #' @details For the local store, this is not a true upload. It is
    #'   just moving a file.
    #' @return `NULL` (invisibly)
    #' @param worker Character of length 1, name of the worker.
    download_output = function(worker) {
      from <- file.path(private$dir_root, "worker_output", worker)
      to <- file.path(private$dir_root, "main_output", worker)
      private$send_file(from = from, to = to)
    },
    #' @description Marshal the data store.
    #' @details Represent the store object as a concise
    #'   character string that can be recovered
    #'   with `parse()` and `eval()` in a different R process.
    #' @return A concise character string that returns the data store
    #'   object when evaluated with `parse()` and `eval()`
    #'   in a different R process.
    marshal = function() {
      expr <- substitute(
        crew:::crew_store_local$new(
          dir_root = dir_root,
          timeout = timeout,
          wait = wait
        ),
        list(
          dir_root = private$dir_root,
          timeout = private$timeout,
          wait = private$wait
        )
      )
      paste(deparse(expr), collapse = "\n")
    }
  )
)
