#' @title Create a controller with a local process launcher.
#' @export
#' @family controllers
#' @description Create an `R6` object to submit tasks and
#'   launch workers on local processes.
#' @inheritParams crew_router
#' @inheritParams crew_launcher_local
#' @inheritParams crew_controller
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' controller <- crew_controller_local()
#' controller$start()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_controller_local <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  seconds_launch = 30,
  seconds_interval = 0.01,
  seconds_timeout = 5,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 0.1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  auto_scale = "demand"
) {
  router <- crew_router(
    name = name,
    workers = workers,
    host = host,
    port = port,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- crew_launcher_local(
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
  controller <- crew_controller(
    router = router,
    launcher = launcher,
    auto_scale = auto_scale
  )
  controller$validate()
  controller
}
