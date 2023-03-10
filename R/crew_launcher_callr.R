#' @title Create a launcher with `callr` workers.
#' @export
#' @keywords internal
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   `callr` workers for a controller.
#' @param seconds_launch Seconds of launchup time to allow.
#'   A worker is unconditionally assumed to be alive
#'   from the moment of its launch until `seconds_launch` seconds later.
#'   After `seconds_launch` seconds, the worker is only
#'   considered alive if it is actively connected to its assign websocket.
#' @param seconds_idle Maximum number of seconds that a worker can idle
#'   since the completion of the last task. If exceeded, the worker exits.
#'   But the timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `idletime` argument of `mirai::server()`.
#' @param seconds_wall Soft wall time in seconds.
#'   The timer does not launch until `tasks_timers` tasks
#'   have completed.
#'   See the `walltime` argument of `mirai::server()`.
#' @param seconds_exit Number of seconds to wait for NNG websockets
#'   to finish sending large data (in case an exit signal is received).
#'   See the `exitdelay` argument of `mirai::server()`.
#' @param seconds_poll_high High polling interval in seconds for the
#'   `mirai` active queue. See the `pollfreqh` argument of
#'   `mirai::server()`.
#' @param seconds_poll_low Low polling interval in seconds for the `mirai`
#'   active queue. See the `pollfreql` argument of
#'   `mirai::server()`.
#' @param tasks_max Maximum number of tasks that a worker will do before
#'   exiting. See the `maxtasks` argument of `mirai::server()`.
#' @param tasks_timers Number of tasks to do before activating
#'   the timers for `seconds_idle` and `seconds_wall`.
#'   See the `timerlaunch` argument of `mirai::server()`.
#' @param async_dial Logical, whether the `mirai` workers should dial in
#'   asynchronously. See the `asyncdial` argument of `mirai::server()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$listen()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets())
#' launcher$launch()
#' m <- mirai::mirai("result")
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' }
crew_launcher_callr <- function(
  seconds_launch = 30,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 0.1,
  seconds_poll_high = 0.005,
  seconds_poll_low = 0.05,
  tasks_max = Inf,
  tasks_timers = 0L,
  async_dial = TRUE
) {
  launcher <- crew_class_launcher_callr$new(
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    seconds_poll_high = seconds_poll_high,
    seconds_poll_low = seconds_poll_low,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
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
#' router$listen()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets())
#' launcher$launch()
#' m <- mirai::mirai("result")
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' }
crew_class_launcher_callr <- R6::R6Class(
  classname = "crew_class_launcher_callr",
  inherit = crew_class_launcher,
  cloneable = FALSE,
  public = list(
    #' @description Launch a `callr` worker to dial into a socket.
    #' @return A `callr::r_bg()` handle.
    #' @param socket Socket where the worker will dial in.
    #' @param token Character of length 1 to identify the instance
    #'   of the `mirai` server process connected to the socket.
    #' @param data Named list of R objects to send to the
    #'   global environment of the worker.
    launch_worker = function(socket, token, data) {
      callr::r_bg(
        func = function (settings, token, data) {
          crew::crew_worker(
            settings = settings,
            token = token,
            data = data
          )
        },
        args = list(
          settings = self$settings(socket),
          token = token,
          data = data
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
