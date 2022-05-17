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
#'   shuffling files to and and the worker file space.
#' @examples
#' # Usage on its own:
#' store <- crew_store_local$new(timeout = Inf)
#' store$write_input("worker", list(value = "x"))
#' store$list_input()
#' store$exists_input("worker")
#' store$read_input("worker")
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
    root = NULL,
    timeout = NULL,
    wait = NULL
  ),
  public = list(
    #' @description Abstract store constructor.
    #' @return An abstract store object.
    #' @param root Character of length 1, root directory of the data store
    #'   on the local file system.
    #' @param timeout Numeric of length 1, number of seconds to wait
    #'   to receive worker input or shuffle files in the worker loop.
    #' @param wait Numeric of length 1, number of seconds to pause between
    #'   iterations waiting for files to arrive in their proper places
    #'   in internal operations.
    initialize = function(
      root = tempfile(),
      timeout = 60,
      wait = 1
    ) {
      crew_true(root, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(timeout, is.numeric(.), length(.) == 1, . >= 0, !anyNA(.))
      crew_true(wait, is.numeric(.), length(.) == 1, . >= 0, !anyNA(.))
      private$root <- root
      private$timeout <- timeout
      private$wait <- wait
    },
    #' @description Get the root directory of the data store.
    #' @return The root directory of the data store.
    get_root = function() {
      private$root
    },
    #' @description Read worker input data.
    #' @return Worker input data.
    #' @param worker Character of length 1, name of the worker.
    read_input = function(worker) {
      private$read_task(direction = "input", worker = worker)
    },
    #' @description Read worker output data.
    #' @return Worker output data.
    #' @param worker Character of length 1, name of the worker.
    read_output = function(worker) {
      private$read_task(direction = "output", worker = worker)
    },
    #' @description Write worker input data to the main file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    #' @param value R object to write.
    write_input = function(worker, value) {
      private$write_task(direction = "input", worker = worker, value = value)
      invisible()
    },
    #' @description Write worker output data to the main file space.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    #' @param value R object to output.
    write_output = function(worker, value) {
      private$write_task(direction = "output", worker = worker, value = value)
      invisible()
    },
    #' @description Check if worker input data exists.
    #' @return Logical of length 1, whether worker input data exists.
    #' @param worker Character of length 1, name of the worker.
    exists_input = function(worker) {
      private$exists_task(direction = "input", worker = worker)
    },
    #' @description Check if worker output data exists.
    #' @return Logical of length 1, whether worker output data exists.
    #' @param worker Character of length 1, name of the worker.
    exists_output = function(worker) {
      private$exists_task(direction = "output", worker = worker)
    },
    #' @description List workers with input data.
    #' @return Character vector of workers with input data.
    list_input = function() {
      private$list_tasks(direction = "input")
    },
    #' @description List workers with output data.
    #' @return Character vector of workers with output data.
    list_output = function() {
      private$list_tasks(direction = "output")
    },
    #' @description Delete worker input data.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    delete_input = function(worker) {
      private$delete_task(direction = "input", worker = worker)
      invisible()
    },
    #' @description Delete worker output data.
    #' @return `NULL` (invisibly).
    #' @param worker Character of length 1, name of the worker.
    delete_output = function(worker) {
      private$delete_task(direction = "output", worker = worker)
      invisible()
    },
    #' @description Get worker log path. Not valid for all stores.
    #' @return Character of length 1, path to the worker log file.
    #' @param worker Character of length 1, name of the worker.
    path_log = function(worker) {
      private$log_worker(worker = worker)
    },
    #' @description Marshal the data store.
    #' @details Represent the store object as a concise
    #'   character string that can be recovered
    #'   with `parse()` and `eval()` in a different R process.
    #' @return A concise character string that returns the data store
    #'   object when evaluated with `parse()` and `eval()`
    #'   in a different R process.
    marshal = function() {
      private$marshal_store()
    },
    #' @description Delete the files of the data store.
    #' @details The local files, along with the main file space,
    #'   are always deleted. Other deletions may depend on the
    #'   specific store subclass.
    #' @return `NULL` (invisibly).
    destroy = function() {
      private$destroy_store()
      invisible()
    }
  )
)
