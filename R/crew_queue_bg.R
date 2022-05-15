#' @title Local background queue
#' @export
#' @aliases crew_queue_bg
#' @family queue
#' @description Task queue with `callr::r_bg()` workers.
#' @details The interface is the same as [crew_queue_session] except
#'   there is a data store (e.g. [crew_store_local]) which
#'   the user can customize. Most users should choose for the session queue
#'   [crew_queue_session] instead of the background queue
#'   [crew_queue_bg]. The only reason to use [crew_queue_bg]
#'   is for subqueues for [crew_queue_future] on clusters
#'   where `batchtools` futures clash with `callr::r_session`.
#' @inheritSection crew_queue Queue attribution
#' @examples
#' fun <- function(x) x + 1
#' args <- list(x = 1)
#' queue <- crew_queue_bg$new(timeout = 60, wait = 0.1)
#' queue$push(fun = fun, args = args)
#' queue$block()
#' result <- queue$pop()
#' str(result)
#' result$result$result
#' queue$shutdown()
#' processx::supervisor_kill()
crew_queue_bg <- R6::R6Class(
  classname = "crew_queue_bg",
  inherit = crew_queue,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    initialize_workers = function(workers) {
      super$initialize_workers(workers)
      private$workers$handle <- map(
        private$workers$worker,
        ~callr::r_bg(
          func = crew_queue_worker_start,
          args = list(
            worker = .x,
            store = private$store$marshal(),
            max_tasks = private$max_tasks,
            timeout = private$timeout,
            wait = private$wait
          ),
          supervise = TRUE,
          stdout = private$store$path_log(worker = .x),
          stderr = private$store$path_log(worker = .x)
        )
      )
      crew_wait(
        fun = ~all(map_lgl(private$workers$handle, ~.x$is_alive())),
        timeout = private$timeout,
        wait = private$wait,
        message = "timed out waiting for bg workers to start."
      )
    },
    worker_start = function(worker) {
      crew_error(sprintf("bg worker %s should already be up.", worker))
    },
    worker_up = function(handle, worker = NULL) {
      length(handle) && handle$is_alive()
    },
    update_crashed = function() {
      crashed <- !map_lgl(private$workers$worker, private$worker_up_log)
      if (any(crashed)) {
        workers <- paste(private$workers$worker[crashed], collapse = ", ")
        crew_error(paste("crashed bg workers:", workers))
      }
    }
  ),
  public = list(
    #' @description Local background queue constructor.
    #' @return A local background queue object.
    #' @param workers Number of workers in the queue.
    #' @param timeout Number of seconds to for a worker to wait
    #'   for something to happen (e.g. the arrival of a task)
    #'   before timing out and quitting.
    #' @param wait Number of seconds to wait in between iterations while
    #'   waiting for something to happen (e.g. the arrival of a task).
    #' @param store A local store object from [crew_store_local].
    initialize = function(
      workers = 1,
      timeout = 60,
      wait = 0.1,
      store = crew_store_local$new(timeout = Inf, wait = wait)
    ) {
      super$initialize(
        workers = workers,
        store = store,
        timeout = timeout,
        wait = wait,
        max_tasks = Inf
      )
    },
    #' @description Shut down the workers.
    #' @return `NULL` (invisibly)
    shutdown = function() {
      super$shutdown()
      for (handle in private$workers$handle) {
        if (length(handle)) {
          handle$kill()
        }
      }
      invisible()
    }
  )
)
