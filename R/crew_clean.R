#' @title Terminate dispatchers and/or workers
#' @export
#' @family utility
#' @description Terminate `mirai` dispatchers and/or `crew` workers
#'   which may be lingering from previous workloads.
#' @details Behind the scenes, `mirai` uses an external R process
#'   called a "dispatcher" to send tasks to `crew` workers.
#'   This dispatcher usually shuts down when you terminate the controller
#'   or quit your R session, but sometimes it lingers. Likewise,
#'   sometimes `crew` workers do not shut down on their own.
#'   The `crew_clean()` function searches the process table on your
#'   local machine and manually terminates any `mirai` dispatchers
#'   and `crew` workers associated with your user name (or the
#'   user name you select in the `user` argument.
#'   Unfortunately, it cannot reach remote workers such as those
#'   launched by a `crew.cluster` controller.
#' @return `NULL` (invisibly). If `verbose` is `TRUE`, it does
#'   print out a message for every terminated process.
#' @param dispatchers Logical of length 1,
#'   whether to terminate dispatchers.
#' @param workers Logical of length 1,
#'   whether to terminate workers.
#' @param user Character of length 1. Terminate dispatchers and/or
#'   workers associated with this user name.
#' @param seconds_interval Seconds to between polling intervals
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
  seconds_interval = 0.5,
  seconds_timeout = 60,
  verbose = TRUE
) {
  # Tests interfere with other processes.
  # Tested in tests/local/test-crew_clean.R.
  # nocov start
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
  ) & rep(dispatchers, nrow(ps))
  ps$worker <- grepl(
    pattern = "crew::crew_worker(",
    x = ps$command,
    fixed = TRUE
  ) & rep(workers, nrow(ps))
  ps <- ps[ps$dispatcher | ps$worker,, drop = FALSE] # nolint
  if (verbose && nrow(ps) < 1L) {
    crew_message("nothing to clean up")
    return(invisible())
  }
  for (index in seq_len(nrow(ps))) {
    handle <- ps$ps_handle[[index]]
    crew_terminate_process(ps::ps_pid(p = handle))
    crew_retry(
      fun = ~!ps::ps_is_running(p = handle),
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
