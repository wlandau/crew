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
crew_store_local <- R6::R6Class(
  classname = "crew_store_local",
  inherit = crew_store,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    read_task = function(direction, worker) {
      crew_true(direction, is.character(.), !anyNA(.), length(.) == 1L)
      path <- file.path(private$root, direction, worker)
      crew_true(
        path,
        is.character(.),
        !anyNA(.),
        length(.) == 1L,
        all(file.exists(.))
      )
      qs::qread(file = path)
    },
    write_task = function(direction, worker, value) {
      crew_true(direction, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(worker, is.character(.), !anyNA(.), length(.) == 1L)
      path_temp <- file.path(private$root, "temp", crew_name())
      path <- file.path(private$root, direction, worker)
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
        fun = function(path, path_temp) {
          file.rename(from = path_temp, to = path)
        },
        args = list(path = path, path_temp = path_temp),
        timeout = private$timeout,
        wait = private$wait,
        message = "store timeout moving local file after writing"
      )
      
      write(paste("ok writing file", path, Sys.getpid()), "~/Desktop/log.txt", append = TRUE)
    },
    exists_task = function(direction, worker) {
      crew_true(direction, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(worker, is.character(.), !anyNA(.), length(.) == 1L)
      all(file.exists(file.path(private$root, direction, worker)))
    },
    list_tasks = function(direction) {
      crew_true(direction, is.character(.), !anyNA(.), length(.) == 1L)
      list.files(file.path(private$root, direction))
    },
    delete_task = function(direction, worker) {
      crew_true(direction, is.character(.), !anyNA(.), length(.) == 1L)
      crew_true(worker, is.character(.), !anyNA(.), length(.) == 1L)
      path <- file.path(private$root, direction, worker)
     path_temp <- file.path(private$root, "temp", crew_name())
     crew_wait(
       fun = function(path, path_temp) file.rename(from = path, to = path_temp),
       args = list(path = path, path_temp = path_temp),
       timeout = private$timeout,
       wait = private$wait,
       message = "store timeout moving local file for deletion"
     )
     unlink(path_temp, recursive = TRUE, force = TRUE)
    },
    marshal_store = function() {
      expr <- substitute(
        crew:::crew_store_local$new(
          root = root,
          timeout = timeout,
          wait = wait
        ),
        list(
          root = private$root,
          timeout = private$timeout,
          wait = private$wait
        )
      )
      paste(deparse(expr), collapse = "\n")
    },
    destroy_store = function() {
      unlink(private$root, recursive = TRUE, force = TRUE)
    }
  )
)
