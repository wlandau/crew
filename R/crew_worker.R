#' @title Crew worker.
#' @export
#' @family utility
#' @description Launches a `crew` worker which runs a `mirai` daemon.
#'   Not a user-side function. Users should not call [crew_worker()]
#'   directly. See launcher plugins like [crew_launcher_local()]
#'   for examples.
#' @return `NULL` (invisibly)
#' @param settings Named list of arguments to `mirai::daemon()`.
#' @param launcher Character of length 1, name of the launcher.
#' @param worker Positive integer of length 1, index of the worker.
#'   This worker index remains the same even when the current instance
#'   of the worker exits and a new instance launches.
#' @param instance Character of length 1 to uniquely identify
#'   the current instance of the worker.
#' @param options_metrics Either `NULL` to opt out of resource metric logging
#'   for workers, or an object from [crew_options_metrics()] to enable
#'   and configure resource metric logging for workers.
crew_worker <- function(
  settings,
  launcher,
  worker,
  instance,
  options_metrics = crew::crew_options_metrics()
) {
  if (!is.null(options_metrics$path)) {
    pids <- Sys.getpid()
    names(pids) <- sprintf("crew_worker_%s_%s_%s", launcher, worker, instance)
    autometric::log_start(
      path = log_metrics_path(options_metrics$path, names(pids)),
      seconds = options_metrics$seconds_interval,
      pids = pids
    )
    on.exit(autometric::log_stop())
  }
  envvars <- c("CREW_LAUNCHER", "CREW_WORKER", "CREW_INSTANCE")
  previous <- Sys.getenv(envvars)
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)), add = TRUE)
  Sys.setenv(
    CREW_LAUNCHER = launcher,
    CREW_WORKER = worker,
    CREW_INSTANCE = instance
  )
  crew_message(utils::capture.output(print(utils::sessionInfo())))
  do.call(what = mirai::daemon, args = settings)
}

log_metrics_path <- function(path, name) {
  if_any(
    is.null(path) || path %in% c("/dev/stdout", "/dev/stderr"),
    path,
    file.path(path, paste0(name, ".log"))
  )
}
