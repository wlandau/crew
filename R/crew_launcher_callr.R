#' @title Create a launcher with `callr` workers.
#' @export
#' @keywords internal
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   `callr` workers for a controller.
#' @param idle_time Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#' @param wall_time Soft wall time in seconds. See the `wall_time`
#'   argument of `mirai::server()`.
#' @param poll_high High polling interval in seconds for the `mirai`
#'   active queue.
#' @param poll_low Low polling interval in seconds for the `mirai`
#'   active queue.
#' @param launch_timeout Number of seconds to time out
#'   waiting for a new group of workers to launch.
#' @param launch_wait Number of seconds to wait between checks
#'   that newly launched workers are ready to receive tasks.
#' @param max_tasks Maximum number of tasks that a worker will do before
#'   exiting.
#' @param async_dial Logical, whether the `mirai` workers should dial in
#'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$connect()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets_listening())
#' launcher$running() # 0
#' launcher$launch()
#' launcher$running() # 1
#' launcher$terminate()
#' launcher$running() # 0
#' router$disconnect()
#' }
crew_launcher_callr <- function(
  idle_time = Inf,
  wall_time = Inf,
  poll_high = 5,
  poll_low = 50,
  launch_timeout = 5,
  launch_wait = 0.1,
  max_tasks = Inf,
  async_dial = TRUE
) {
  launcher <- crew_class_launcher_callr$new(
    sockets = character(0),
    workers = list(),
    idle_time = idle_time,
    wall_time = wall_time,
    poll_high = poll_high,
    poll_low = poll_low,
    launch_timeout = launch_timeout,
    launch_wait = launch_wait,
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
#' @details See [crew_launcher_callr()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$connect()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets_listening())
#' launcher$running() # 0
#' launcher$launch()
#' launcher$running() # 1
#' launcher$terminate()
#' launcher$running() # 0
#' router$disconnect()
#' }
crew_class_launcher_callr <- R6::R6Class(
  classname = c("crew_class_launcher_callr"),
  cloneable = FALSE,
  public = list(
    #' @field sockets Websockets for listening to the workers.
    sockets = NULL,
    #' @field workers List of `callr::r_bg()` handles for workers.
    #'   The handle is `NA` if it is not yet called.
    workers = NULL,
    #' @field idle_time Argument to `crew_launcher_callr()`.
    idle_time = NULL,
    #' @field wall_time Argument to `crew_launcher_callr()`.
    wall_time = NULL,
    #' @field poll_high Argument to `crew_launcher_callr()`.
    poll_high = NULL,
    #' @field poll_low Argument to `crew_launcher_callr()`.
    poll_low = NULL,
    #' @field launch_timeout Argument to `crew_launcher_callr()`.
    launch_timeout = NULL,
    #' @field launch_wait Argument to `crew_launcher_callr()`.
    launch_wait = NULL,
    #' @field max_tasks Argument to `crew_launcher_callr()`.
    max_tasks = NULL,
    #' @field async_dial Argument to `crew_launcher_callr()`.
    async_dial = NULL,
    #' @description `mirai` launcher constructor.
    #' @return An `R6` object with the launcher.
    #' @param sockets Argument to `crew_launcher_callr()`.
    #' @param workers Argument to `crew_launcher_callr()`.
    #' @param idle_time Argument to `crew_launcher_callr()`.
    #' @param wall_time Argument to `crew_launcher_callr()`.
    #' @param poll_high Argument to `crew_launcher_callr()`.
    #' @param poll_low Argument to `crew_launcher_callr()`.
    #' @param launch_timeout Argument to `crew_launcher_callr()`.
    #' @param launch_wait Argument to `crew_launcher_callr()`.
    #' @param max_tasks Argument to `crew_launcher_callr()`.
    #' @param async_dial Argument to `crew_launcher_callr()`.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' router$connect()
    #' launcher <- crew_launcher_callr()
    #' launcher$populate(sockets = router$sockets_listening())
    #' launcher$running() # 0
    #' launcher$launch()
    #' launcher$running() # 1
    #' launcher$terminate()
    #' launcher$running() # 0
    #' router$disconnect()
    #' }
    initialize = function(
      sockets = NULL,
      workers = NULL,
      idle_time = NULL,
      wall_time = NULL,
      poll_high = NULL,
      poll_low = NULL,
      launch_timeout = NULL,
      launch_wait = NULL,
      max_tasks = NULL,
      async_dial = NULL
    ) {
      self$sockets <- sockets
      self$workers <- workers
      self$idle_time <- idle_time
      self$wall_time <- wall_time
      self$poll_high <- poll_high
      self$poll_low <- poll_low
      self$launch_timeout <- launch_timeout
      self$launch_wait <- launch_wait
      self$max_tasks <- max_tasks
      self$async_dial <- async_dial
    },
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$sockets, is.character(.), nzchar(.), !anyNA(.))
      true(self$workers, is.list(.), length(.) == length(self$sockets))
      fields <- c(
        "idle_time",
        "wall_time",
        "poll_high",
        "poll_low",
        "launch_timeout",
        "launch_wait",
        "max_tasks"
      )
      for (field in fields) {
        true(self[[field]], is.numeric(.), . > 0, length(.) == 1L, !anyNA(.))
      }
      true(self$async_dial, isTRUE(.) || isFALSE(.))
      invisible()
    },
    #' @description Populate workers.
    #' @return `NULL` (invisibly).
    #' @param sockets Character vector of local websockets that the workers
    #'   will use to dial in to receive tasks.
    populate = function(sockets = character(0)) {
      true(sockets, length(.) > 0L, is.character(.), nzchar(.), !anyNA(.))
      self$sockets <- sockets
      self$workers <- as.list(rep(NA, length(sockets)))
      self$validate()
      invisible()
    },
    #' @description Launch one or more workers.
    #' @details The actual number of newly launched workers
    #'   is less than or equal to the number of sockets
    #'   supplied to `$populate()` minus the number of workers
    #'   already running.
    #' @return `NULL` (invisibly).
    #' @param n Maximum number of workers to launch.
    launch = function(n = 1L) {
      available <- map_lgl(x = self$workers, f = ~!process_running(.x))
      index <- utils::head(which(available), n = n)
      self$workers[index] <- map(
        x = index,
        f = ~callr::r_bg(
          func = \(...) do.call(what = mirai::server, args = list(...)),
          args = list(
            url = self$sockets[.x],
            idletime = self$idle_time * 1000,
            walltime = self$wall_time * 1000,
            tasklimit = self$max_tasks,
            pollfreqh = self$poll_high * 1000,
            pollfreql = self$poll_low * 1000,
            asyncdial = self$async_dial
          )
        )
      )
      invisible()
    },
    #' @description Terminate all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      lapply(self$workers, \(x) if (inherits(x, "r_process")) x$kill())
      invisible()
    }
  )
)
