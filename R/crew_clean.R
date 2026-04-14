#' @title Deprecated: terminate dispatchers and/or workers
#' @export
#' @family utility
#' @description Deprecated on 2025-08-26 in `crew` version 1.2.1.9006.
#'   Please use [crew_monitor_local()] instead.
#' @return `NULL` (invisibly). If `verbose` is `TRUE`, it does
#'   print out a message for every terminated process.
#' @param dispatchers Logical of length 1,
#'   whether to terminate dispatcher processes
#'   (in older versions of `mirai`: <= 2.6.1).
#'   In `mirai` > 2.6.1, the dispatcher is a thread within
#'   the controller's own R process
#'   and will not be terminated by this function.
#' @param workers Logical of length 1,
#'   whether to terminate workers.
#' @param user Character of length 1. Terminate processes
#'   associated with this user name.
#' @param seconds_interval Seconds to wait between polling intervals
#'   waiting for a process to exit.
#' @param seconds_timeout Seconds to wait for a process to exit.
#' @param verbose Logical of length 1, whether to print an informative
#'   message every time a process is terminated.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_clean()
#' }
crew_clean <- function(
  dispatchers = TRUE,
  workers = TRUE,
  user = ps::ps_username(),
  seconds_interval = 0.25,
  seconds_timeout = 60,
  verbose = TRUE
) {
  # Tests interfere with other processes.
  # Tested in tests/local/test-crew_clean.R.
  # nocov start
  crew_deprecate(
    name = "crew_clean()",
    date = "2025-08-26",
    version = "1.2.1.9006",
    alternative = "crew_monitor_local()",
    condition = "warning"
  )
  crew_assert(dispatchers, isTRUE(.) || isFALSE(.))
  crew_assert(workers, isTRUE(.) || isFALSE(.))
  crew_assert(user, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  crew_assert(verbose, isTRUE(.) || isFALSE(.))
  ps <- ps::ps(user = user)
  ps$command <- map_chr(x = ps$ps_handle, f = process_command)
  ps$dispatcher <- grepl(
    pattern = "mirai::dispatcher(",
    x = ps$command,
    fixed = TRUE
  ) &
    rep(dispatchers, nrow(ps))
  ps$worker <- grepl(
    pattern = "crew::crew_worker(",
    x = ps$command,
    fixed = TRUE
  ) &
    rep(workers, nrow(ps))
  ps <- ps[ps$dispatcher | ps$worker, , drop = FALSE] # nolint
  if (verbose && nrow(ps) < 1L) {
    crew_message("nothing to clean up")
    return(invisible())
  }
  for (index in seq_len(nrow(ps))) {
    handle <- ps$ps_handle[[index]]
    crew_terminate_process(ps::ps_pid(p = handle))
    crew_retry(
      fun = ~ !ps::ps_is_running(p = handle),
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    if (verbose) {
      message <- sprintf(
        "terminated PID %s %s",
        ps$pid[index],
        if_any(ps$dispatcher[index], "dispatcher", "worker")
      )
      crew_message(message)
    }
  }
  invisible()
  # nocov end
}

process_command <- function(handle) {
  command <- tryCatch(ps::ps_cmdline(handle), error = function(condition) " ")
  paste(command, collapse = " ")
}
