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
  classname = "crew_class_launcher_callr",
  inherit = crew_class_launcher,
  cloneable = FALSE,
  public = list(
    #' @field processes List of `callr` process handles.
    processes = list(),
    #' @description Validate the launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      super$validate()
      walk(self$processes, ~true(.x, inherits(., "r_process")))
      invisible()
    },
    #' @description Launch one or more workers.
    #' @details The actual number of newly launched workers
    #'   is less than or equal to the number of sockets
    #'   supplied to `$populate()` minus the number of workers
    #'   already running.
    #' @return `NULL` (invisibly).
    #' @param sockets Sockets where the workers will dial in.
    launch = function(sockets = character(0)) {
      new_processes <- map(
        x = sockets,
        f = ~callr::r_bg(
          func = \(...) do.call(what = mirai::server, args = list(...)),
          args = list(
            url = .x,
            idletime = self$idle_time * 1000,
            walltime = self$wall_time * 1000,
            tasklimit = self$max_tasks,
            pollfreqh = self$poll_high * 1000,
            pollfreql = self$poll_low * 1000,
            asyncdial = self$async_dial
          )
        )
      )
      self$processes <- c(self$processes, new_processes)
      invisible()
    },
    #' @description Terminate all workers.
    #' @return `NULL` (invisibly).
    terminate = function() {
      walk(self$processes, ~.x$kill())
    }
  )
)
