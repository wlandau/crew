#' @title Create a launcher with local process workers.
#' @export
#' @family launchers
#' @description Create an `R6` object to launch and maintain
#'   local process workers.
#' @inheritParams crew_launcher
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' router$start()
#' launcher <- crew_launcher_local()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result", .compute = router$name)
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' crew_session_terminate()
#' }
crew_launcher_local <- function(
  name = NULL,
  seconds_launch = 30,
  seconds_interval = 0.001,
  seconds_timeout = 10,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE
) {
  name <- as.character(name %|||% crew_random_name())
  launcher <- crew_class_launcher_local$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection
  )
  launcher$validate()
  launcher
}

#' @title Local process launcher class
#' @export
#' @family launchers
#' @description `R6` class to launch and manage local process workers.
#' @details See [crew_launcher_local()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' router <- crew_router()
#' router$start()
#' launcher <- crew_launcher_local()
#' launcher$populate(sockets = router$sockets)
#' launcher$launch()
#' m <- mirai::mirai("result", .compute = router$name)
#' Sys.sleep(0.25)
#' m$data
#' router$terminate()
#' crew_session_terminate()
#' }
crew_class_launcher_local <- R6::R6Class(
  classname = "crew_class_launcher_local",
  inherit = crew_class_launcher,
  cloneable = FALSE,
  public = list(
    #' @description Launch a local process worker which will
    #'   dial into a socket.
    #' @details The `call` argument is R code that will run to
    #'   initiate the worker. Together, the `launcher`, `worker`,
    #'   and `instance` arguments are useful for
    #'   constructing informative job names.
    #' @return A handle object to allow the termination of the worker
    #'   later on.
    #' @param call Text string with a namespaced call to [crew_worker()]
    #'   which will run in the worker and accept tasks.
    #' @param launcher Character of length 1, name of the launcher.
    #' @param worker Positive integer of length 1, index of the worker.
    #'   This worker index remains the same even when the current instance
    #'   of the worker exits and a new instance launches.
    #'   It is always between 1 and the maximum number of concurrent workers.
    #' @param instance Character of length 1 to uniquely identify
    #'   the current instance of the worker.
    launch_worker = function(call, launcher, worker, instance) {
      bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
      path <- file.path(R.home("bin"), bin)
      processx::process$new(command = path, args = c("-e", call))
    },
    #' @description Terminate a local process worker.
    #' @return `NULL` (invisibly).
    #' @param handle A process handle object previously
    #'   returned by `launch_worker()`.
    terminate_worker = function(handle) {
      handle$kill()
      invisible()
    }
  )
)
