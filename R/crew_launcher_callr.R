#' @title Create a launcher with `callr` workers.
#' @export
#' @keywords internal
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   `callr` workers for a controller.
#' @param seconds_start Seconds of startup time to allow.
#'   A worker is unconditionally assumed to be alive
#'   from the moment of its launch until `seconds_start` seconds later.
#'   After `seconds_start` seconds, the worker is only
#'   considered alive if it is actively connected to its assign websocket.
#' @param seconds_idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#' @param seconds_wall Soft wall time in seconds. See the `walltime`
#'   argument of `mirai::server()`.
#' @param seconds_poll_high High polling interval in seconds for the `mirai`
#'   active queue.
#' @param seconds_poll_low Low polling interval in seconds for the `mirai`
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
  seconds_start = 1,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_poll_high = 0.005,
  seconds_poll_low = 0.05,
  max_tasks = Inf,
  async_dial = TRUE
) {
  launcher <- crew_class_launcher_callr$new(
    seconds_start = seconds_start,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_poll_high = seconds_poll_high,
    seconds_poll_low = seconds_poll_low,
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
    #' @description Launch a `callr` worker to dial into a socket.
    #' @return A `callr::r_bg()` handle.
    #' @param socket Socket where the worker will dial in.
    launch_worker = function(socket) {
      callr::r_bg(
        func = \(...) do.call(what = mirai::server, args = list(...)),
        args = list(
          url = socket,
          idletime = self$seconds_idle * 1000,
          walltime = self$seconds_wall * 1000,
          tasklimit = self$max_tasks,
          pollfreqh = self$seconds_poll_high * 1000,
          pollfreql = self$seconds_poll_low * 1000,
          asyncdial = self$async_dial
        )
      )
    },
    #' @description Terminate a `callr` worker.
    #' @return `NULL` (invisibly).
    #' @param handle A `callr` process handle previously
    #'   returned by `launch_worker()`.
    terminate_worker = function(handle) {
      handle$kill()
    }
  )
)
