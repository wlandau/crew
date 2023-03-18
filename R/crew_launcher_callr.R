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
  seconds_exit = 0.1,
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
    #' @param socket Socket where the worker will receive tasks.
    #' @param host IP address of the `mirai` client that sends tasks.
    #' @param port TCP port to register a successful connection
    #'   to the host. Different from that of `socket`.
    #' @param token Character of length 1 to identify the instance
    #'   of the `mirai` server process connected to the socket.
    #' @param name User-supplied name of the launcher, useful for
    #'   constructing informative job labels.
    launch_worker = function(socket, host, port, token, name) {
      callr::r_bg(
        # This part is covered by tests but unreachable by covr:
        # nocov start
        func = function(settings, host, port, token) {
          crew::crew_worker(
            settings = settings,
            host = host,
            port = port,
            token = token
          )
        },
        # nocov end
        args = list(
          settings = self$settings(socket),
          host = host,
          port = port,
          token = token
        )
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
