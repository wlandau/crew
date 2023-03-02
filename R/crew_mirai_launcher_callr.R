#' @title Create a `mirai` launcher with `callr` workers.
#' @export
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   `callr` workers for a `mirai` controller.
#' @param idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#' @param wall_time Soft wall time in seconds. See the `wall_time`
#'   argument of `mirai::server()`.
#' @param poll_high High polling interval in seconds for the `mirai`
#'   active queue.
#' @param poll_low Low polling interval in seconds for the `mirai`
#'   active queue.
#' @param poll_launch Maximum number of seconds to wait for a new group
#'   of workers to launch.
#' @param max_tasks Maximum number of tasks that a worker will do before
#'   exiting.
#' @param async_dial Logical, whether the `mirai` workers should dial in
#'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' launcher <- crew_mirai_launcher_callr()
#' launcher$validate()
#' }
crew_mirai_launcher_callr <- function(
  idle = Inf,
  wall_time = Inf,
  poll_high = 5,
  poll_low = 50,
  poll_launch = 5,
  max_tasks = Inf,
  async_dial = TRUE
) {
  launcher <- crew_class_mirai_launcher_callr$new(
    sockets = character(0),
    workers = list(),
    idle = idle,
    wall_time = wall_time,
    poll_high = poll_high,
    poll_low = poll_low,
    poll_launch = poll_launch,
    max_tasks = max_tasks,
    async_dial = async_dial
  )
  launcher$validate()
  launcher
}

#' @title `mirai` launcher class
#' @export
#' @family launchers
#' @description `R6` class to launch and manage `mirai` workers.
#' @details See [crew_mirai_launcher_callr()].
crew_class_mirai_launcher_callr <- R6::R6Class(
  classname = "crew_class_mirai_launcher_callr",
  cloneable = FALSE,
  public = list(
    #' @field sockets TCP sockets for listening to the workers.
    sockets = NULL,
    #' @field workers List of `callr::r_session()` handles for workers.
    #'   The handle is `NA` if it is not yet called.
    workers = NULL,
    #' @field idle Argument to `crew_mirai_launcher_callr()`.
    idle = NULL,
    #' @field wall_time Argument to `crew_mirai_launcher_callr()`.
    wall_time = NULL,
    #' @field poll_high Argument to `crew_mirai_launcher_callr()`.
    poll_high = NULL,
    #' @field poll_low Argument to `crew_mirai_launcher_callr()`.
    poll_low = NULL,
    #' @field poll_launch Argument to `crew_mirai_launcher_callr()`.
    poll_launch = NULL,
    #' @field max_tasks Argument to `crew_mirai_launcher_callr()`.
    max_tasks = NULL,
    #' @field async_dial Argument to `crew_mirai_launcher_callr()`.
    async_dial = NULL,
    #' @description `mirai` launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param sockets Argument to `crew_mirai_launcher_callr()`.
    #' @param workers Argument to `crew_mirai_launcher_callr()`.
    #' @param idle Argument to `crew_mirai_launcher_callr()`.
    #' @param wall_time Argument to `crew_mirai_launcher_callr()`.
    #' @param poll_high Argument to `crew_mirai_launcher_callr()`.
    #' @param poll_low Argument to `crew_mirai_launcher_callr()`.
    #' @param poll_launch Argument to `crew_mirai_launcher_callr()`.
    #' @param max_tasks Argument to `crew_mirai_launcher_callr()`.
    #' @param async_dial Argument to `crew_mirai_launcher_callr()`.
    initialize = function(
      sockets = NULL,
      workers = NULL,
      idle = NULL,
      wall_time = NULL,
      poll_high = NULL,
      poll_low = NULL,
      poll_launch = NULL,
      max_tasks = NULL,
      async_dial = NULL
    ) {
      self$sockets <- sockets
      self$workers <- workers
      self$idle <- idle
      self$wall_time <- wall_time
      self$poll_high <- poll_high
      self$poll_low <- poll_low
      self$poll_launch <- poll_launch
      self$max_tasks <- max_tasks
      self$async_dial <- async_dial
    },
    #' @description Disconnect at garbage collection time.
    #' @return `NULL` (invisibly).
    finalize = function() {
      self$terminate()
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$sockets, is.character(.), length(.) > 0L, nzchar(.), !anyNA(.))
      true(self$workers, is.list(.), length(.) == length(self$sockets))
      fields <- c(
        "idle",
        "wall_time",
        "poll_high",
        "poll_low",
        "poll_launch",
        "max_tasks"
      )
      for (field in fields) {
        true(self[[field]], is.numeric(.), . > 0, length(.) == 1L, !anyNA(.))
      }
      expect_true(self$async_dial, isTRUE(.) || isFALSE(.))
      invisible()
    },
    #' @description Populate workers.
    #' @return `NULL` (invisibly).
    #' @param sockets Character vector of local TCP sockets that the workers
    #'   will use to dial in to receive tasks.
    populate = function(sockets = character(0)) {
      true(sockets, length(.) > 0L, is.character(.), nzchar(.), !anyNA(.))
      self$sockets <- sockets
      self$workers <- as.list(rep(NA, length(sockets)))
      invisible()
    },
    #' @description Count the number of running workers.
    #' @return Number of running workers.
    running = function() {
      sum(vapply(self$workers, \(x) x$is_alive(), VALUE = logical(1L)))
    },
    #' @description Launch one or more workers.
    #' @details The actual number of newly launched workers
    #'   is less than or equal to the number of sockets
    #'   supplied to `$populate()` minus the number of workers
    #'   already running.
    #' @return `NULL` (invisibly).
    #' @param n Maximum number of workers to launch.
    launch = function(n = 1L) {
      launched <- integer(0)
      for (index in seq_along(self$workers)) {
        worker <- self$workers[[index]]
        if (!is.na(worker) && !worker$is_alive()) {
          self$workers[[index]] <- callr::r_session$new(wait = FALSE)
          launched <- c(index, launched)
          if (length(launched) >= n) {
            break
          }
        }
      }
      if (length(launched)) {
        con <- lapply(
          self$workers[launched],
          \(x) x$get_poll_connection()
        )
        processx::poll(processes = con, ms = 1000 * self$poll_launch)
      }
      for (index in launched) {
        self$workers[[index]]$call(
          func = \(...) do.call(what = mirai::server, args = list(...)),
          args = list(
            url = self$sockets[index],
            idletime = self$idle,
            walltime = self$wall_time,
            tasklimit = self$max_tasks,
            pollfreqh = self$poll_high,
            pollfreql = self$poll_low,
            asyncdial = self$async_dial
          )
        )
      }
      invisible()
    },
    #' @description Terminate all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      lapply(self$workers, \(x) if (!is.na(x)) x$kill())
      invisible()
    }
  )
)
