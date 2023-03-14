#' @title Create a controller with a callr launcher.
#' @export
#' @family controllers
#' @description Create an `R6` object to submit tasks and
#'   launch `callr` workers.
#' @inheritParams crew_router
#' @inheritParams crew_launcher_callr
#' @inheritParams crew_controller
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' controller <- crew_controller_callr()
#' controller$connect()
#' controller$push(name = "task", command = sqrt(4))
#' controller$wait()
#' controller$pop()
#' controller$terminate()
#' }
crew_controller_callr <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = 0L,
  router_timeout = 5,
  router_wait = 0.1,
  seconds_exit = 0.1,
  seconds_poll_high = 0.01,
  seconds_poll_low = 0.1,
  async_dial = TRUE,
  seconds_launch = 30,
  seconds_idle = Inf,
  seconds_wall = Inf,
  tasks_max = Inf,
  tasks_timers = 0L,
  cleanup = FALSE,
  auto_scale = "demand"
) {
  router <- crew_router(
    name = name,
    workers = workers,
    host = host,
    port = port,
    router_timeout = router_timeout,
    router_wait = router_wait,
    seconds_exit = seconds_exit,
    seconds_poll_high = seconds_poll_high,
    seconds_poll_low = seconds_poll_low,
    async_dial = async_dial
  )
  launcher <- crew_launcher_callr(
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    async_dial = async_dial,
    cleanup = cleanup
  )
  controller <- crew_controller(
    router = router,
    launcher = launcher,
    auto_scale = auto_scale
  )
  controller$validate()
  controller
}
