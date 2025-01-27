#' @title Crew worker.
#' @export
#' @family utility
#' @description Launches a `crew` worker which runs a `mirai` daemon.
#'   Not a user-side function. Users should not call [crew_worker()]
#'   directly. See launcher plugins like [crew_launcher_local()]
#'   for examples.
#' @return `NULL` (invisibly)
#' @param settings Named list of arguments to `mirai::daemon()`.
#' @param launcher Character string, name of the launcher
#' @param worker Character of length 1 to uniquely identify the current worker.
#' @param options_metrics Either `NULL` to opt out of resource metric logging
#'   for workers, or an object from [crew_options_metrics()] to enable
#'   and configure resource metric logging for workers.
#'   For resource logging to run,
#'   the `autometric` R package version 0.1.0 or higher
#'   must be installed.
crew_worker <- function(
  settings,
  launcher,
  worker,
  options_metrics = crew::crew_options_metrics()
) {
  autometric <- package_installed("autometric (>= 0.1.0)")
  if (autometric && !is.null(options_metrics$path)) {
    pids <- Sys.getpid()
    names(pids) <- sprintf("crew_worker_%s_%s", launcher, worker)
    autometric::log_start(
      path = log_metrics_path(options_metrics$path, names(pids)),
      seconds = options_metrics$seconds_interval,
      pids = pids
    )
    on.exit(autometric::log_stop())
  }
  envvars <- c("CREW_CONTROLLER", "CREW_WORKER")
  previous <- Sys.getenv(envvars)
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)), add = TRUE)
  Sys.setenv(CREW_CONTROLLER = launcher, CREW_WORKER = worker)
  crew_message(utils::capture.output(print(utils::sessionInfo())))
  code <- do.call(what = mirai::daemon, args = settings)
  crew_message(
    paste(
      "crew worker finished normally with mirai message:",
      mirai::nextcode(xc = code)
    )
  )
}

log_metrics_path <- function(path, name) {
  if_any(
    is.null(path) || path %in% c("/dev/stdout", "/dev/stderr"),
    path,
    file.path(path, paste0(name, ".log"))
  )
}
