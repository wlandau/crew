#' @title Create a `mirai` controller with a callr launcher.
#' @export
#' @family controllers
#' @description Create an `R6` object to submit tasks and launch workers
#'   with a `mirai` task queue and `callr` workers
#' @inheritParams crew_mirai_router
#' @inheritParams crew_mirai_launcher_callr
#' @inheritParams crew_mirai_controller
crew_mirai_controller_callr <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  ports = NULL,
  router_timeout = 5,
  router_wait = 0.1,
  idle_time = Inf,
  wall_time = Inf,
  poll_high = 5,
  poll_low = 50,
  launch_timeout = 5,
  launch_wait = 0.1,
  max_tasks = Inf,
  async_dial = TRUE,
  scale_method = "demand"
) {
  router <- crew_mirai_router(
    name = name,
    workers = workers,
    host = host,
    ports = ports,
    router_timeout = router_timeout,
    router_wait = router_wait
  )
  launcher <- crew_mirai_launcher_callr(
    idle_time = idle_time,
    wall_time = wall_time,
    poll_high = poll_high,
    poll_low = poll_low,
    launch_timeout = launch_timeout,
    launch_wait = launch_wait,
    max_tasks = max_tasks,
    async_dial = async_dial
  )
  controller <- crew_mirai_controller(
    router = router,
    launcher = launcher,
    scale_method = scale_method
  )
  controller$validate()
  controller
}
