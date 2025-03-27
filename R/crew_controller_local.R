#' @title Create a controller with a local process launcher.
#' @export
#' @family plugin_local
#' @description Create an `R6` object to submit tasks and
#'   launch workers on local processes.
#' @inheritParams crew_client
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
  host = "127.0.0.1",
  port = NULL,
  tls = crew::crew_tls(),
  tls_enable = NULL,
  tls_config = NULL,
  serialization = NULL,
  seconds_interval = 1,
  seconds_timeout = 60,
  seconds_launch = 30,
  seconds_idle = 300,
  seconds_wall = Inf,
  seconds_exit = NULL,
  retry_tasks = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  crashes_error = NULL,
  launch_max = NULL,
  r_arguments = c("--no-save", "--no-restore"),
  crashes_max = 5L,
  backup = NULL,
  options_metrics = crew::crew_options_metrics(),
  options_local = crew::crew_options_local(),
  local_log_directory = NULL,
  local_log_join = NULL
) {
  crew_deprecate(
    name = "seconds_exit",
    date = "2023-09-21",
    version = "0.5.0.9002",
    alternative = "none (no longer necessary)",
    condition = "warning",
    value = seconds_exit
  )
  crew_deprecate(
    name = "local_log_directory",
    date = "2024-10-8",
    version = "0.9.5.9012",
    alternative = "options_local argument",
    condition = "warning",
    value = local_log_directory
  )
  crew_deprecate(
    name = "local_log_join",
    date = "2024-10-8",
    version = "0.9.5.9012",
    alternative = "options_local argument",
    condition = "warning",
    value = local_log_join
  )
  crew_deprecate(
    name = "launch_max",
    date = "2024-11-04",
    version = "0.10.1.9000",
    alternative = "none",
    condition = "warning",
    value = launch_max
  )
  crew_deprecate(
    name = "crashes_error",
    date = "2025-01-13",
    version = "0.10.2.9002",
    alternative = "none",
    condition = "message",
    value = crashes_error
  )
  crew_deprecate(
    name = "retry_tasks",
    date = "2025-01-24",
    version = "0.10.2.9005",
    alternative = "none",
    condition = "message",
    value = retry_tasks
  )
  options_local$log_directory <- local_log_directory %|||%
    options_local$log_directory
  options_local$log_join <- local_log_join %|||%
    options_local$log_join
  client <- crew_client(
    host = host,
    port = port,
    tls = tls,
    tls_enable = tls_enable,
    tls_config = tls_config,
    serialization = serialization,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- crew_launcher_local(
    name = name,
    workers = workers,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    tls = tls,
    r_arguments = r_arguments,
    options_metrics = options_metrics,
    options_local = options_local,
    local_log_directory = local_log_directory,
    local_log_join = local_log_join
  )
  controller <- crew_controller(
    client = client,
    launcher = launcher,
    crashes_max = crashes_max,
    backup = backup
  )
  controller$validate()
  controller
}
