#' @title Create a launcher with `callr` workers.
#' @export
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   `callr` workers for a controller.
#' @inheritParams crew_launcher
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' router$listen()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result")
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' crew_session_terminate()
#' }
crew_launcher_callr <- function(
  name = NULL,
  seconds_launch = 30,
  seconds_interval = 0.001,
  seconds_timeout = 10,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 1,
  tasks_max = Inf,
  tasks_timers = 0L,
  async_dial = TRUE,
  cleanup = FALSE
) {
  name <- as.character(name %|||% random_name())
  launcher <- crew_class_launcher_callr$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    async_dial = async_dial,
    cleanup = cleanup
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
#' crew_session_start()
#' router <- crew_router()
#' router$listen()
#' launcher <- crew_launcher_callr()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result")
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' crew_session_terminate()
#' }
crew_class_launcher_callr <- R6::R6Class(
  classname = "crew_class_launcher_callr",
  inherit = crew_class_launcher,
  cloneable = FALSE,
  public = list(
    #' @description Launch a `callr` worker to dial into a socket.
    #' @return A `callr::r_bg()` handle.
    #' @param call Text string with a namespaced call to [crew_worker()]
    #'   which will run in the worker and accept tasks.
    #' @param name Text string with the name of the launcher.
    #' @param token Text string to uniquely identify the new instance
    #'   of the worker process.
    launch_worker = function(call, name, token) {
      callr::r_bg(
        func = function(call) eval(parse(text = call)),
        args = list(call = call)
      )
    },
    #' @description Terminate a `callr` worker.
    #' @return `NULL` (invisibly).
    #' @param handle A `callr` process handle previously
    #'   returned by `launch_worker()`.
    terminate_worker = function(handle) {
      handle$kill()
      crew_wait(
        ~!handle$is_alive(),
        seconds_interval = self$seconds_interval,
        seconds_timeout = self$seconds_timeout
      )
      invisible()
    }
  )
)
