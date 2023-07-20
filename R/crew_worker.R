#' @title Crew worker.
#' @export
#' @family utilities
#' @description Launch a `crew` worker which runs a `mirai` daemon.
#' @return `NULL` (invisibly)
#' @param settings Named list of arguments to `mirai::daemon()`.
#' @param launcher Character of length 1, name of the launcher.
#' @param worker Positive integer of length 1, index of the worker.
#'   This worker index remains the same even when the current instance
#'   of the worker exits and a new instance launches.
#' @param instance Character of length 1 to uniquely identify
#'   the current instance of the worker.
crew_worker <- function(settings, launcher, worker, instance) {
  envvars <- c("CREW_LAUNCHER", "CREW_WORKER", "CREW_INSTANCE")
  previous <- Sys.getenv(envvars)
  on.exit(do.call(what = Sys.setenv, args = as.list(previous)))
  Sys.setenv(
    CREW_LAUNCHER = launcher,
    CREW_WORKER = worker,
    CREW_INSTANCE = instance
  )
  do.call(what = mirai::daemon, args = settings)
}
