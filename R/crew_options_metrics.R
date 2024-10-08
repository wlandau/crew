#' @title Options for logging resource usage metrics.
#' @export
#' @family options
#' @description If a [crew_options_metrics()] object is
#'   supplied to the `options_metrics` argument of a `crew`
#'   controller function, then the `autometric` R package will
#'   record resource usage metrics (such as CPU and memory usage)
#'   as the controller and workers are running. Logging happens in
#'   the background (through a detached POSIX) so as not to disrupt
#'   the R session. On Unix-like systems, [crew_options_metrics()]
#'   can specify `/dev/stdout` or `/dev/stderr` as the log files, which will
#'   redirect output to existing logs you are already using. Then,
#'   even if those logs are mixed with other messages, you can use functions
#'   [autometric::log_read()] and [autometric::log_plot()] to read and
#'   visualize resource usage data.
#' @return A classed list of options for logging resource usage metrics.
#' @param path_local Where to write resource metric log entries for the
#'   local R session and `mirai` dispatcher process.
#'   If `NULL`, resource metric logging is turned off for these processes.
#'   If `"/dev/stdout"` or `"/dev/stderr"`, resource usage metrics
#'   are printed to standard output or standard error,
#'   respectively. If a different character string, that string is the
#'   directory path for writing logs to files on disk, and each
#'   controller instance gets a different log file.
#'   [autometric::log_read()] and [autometric::log_plot()] can read and
#'   visualize data from logs, even if the logs files are mixed
#'   with other kinds of messages.
#' @param path_workers Like `path_local`, but for the `crew` worker processes.
#'   On Unix-like systems, it is recommended to set `path_workers`
#'   equal to `"/dev/stdout"` or `"/dev/stderr"` to automatically write
#'   resource log messages to the existing log files generated on your
#'   platform (for example, the logs configured with
#'   [crew_options_local()] in the case of [crew_controller_local()]).
#'   [autometric::log_read()] and [autometric::log_plot()] can read and
#'   visualize data from logs, even if the logs files are mixed
#'   with other kinds of messages.
#' @param seconds_local Positive number, seconds between resource metric log
#'   entries written to `path_local`.
#' @param seconds_workers Positive number, seconds between resource metric log
#'   entries written to `seconds_workers`.
#' @examples
#'   crew_options_metrics()
crew_options_metrics = function(
  path_local = NULL,
  path_workers = NULL,
  seconds_local = 5,
  seconds_workers = 5
) {
  out <- structure(
    list(
      path_local = path_local,
      path_workers = path_workers,
      seconds_local = seconds_local,
      seconds_workers = seconds_workers
    ),
    class = c("crew_options_metrics", "crew_options")
  )
  crew_options_metrics_validate(out)
  out
}

crew_options_metrics_validate <- function(options) {
  crew_assert(
    inherits(options, "crew_options_metrics"),
    message = "options_metrics object must come from crew_options_metrics()."
  )
  crew_assert(
    options$path_local %|||% "x",
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    nzchar(.),
    message = paste(
      "path_local must be NULL, \"/dev/stdout\", ",
      "\"/dev/stderr\", or a valid directory path."
    )
  )
  crew_assert(
    options$path_workers %|||% "x",
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    nzchar(.),
    message = paste(
      "path_workers must be NULL, \"/dev/stdout\", ",
      "\"/dev/stderr\", or a valid directory path."
    )
  )
  on_windows <- identical(unname(tolower(Sys.info()[["sysname"]])), "windows")
  streams <- file.path("/dev", c("stdout", "stderr"))
  crew_assert(
    !(on_windows && options$path_local %in% streams),
    message = paste(
      "path_local cannot be \"/dev/stdout\" or \"/dev/stderr\" on Windows."
    )
  )
  crew_assert(
    !(on_windows && options$path_workers %in% streams),
    message = paste(
      "path_workers cannot be \"/dev/stdout\" or \"/dev/stderr\" on Windows."
    )
  )
  crew_assert(
    options$seconds_local,
    is.numeric(.),
    length(.) == 1,
    is.finite(.),
    . > 0,
    message = "seconds_local must be a positive number."
  )
  crew_assert(
    options$seconds_workers,
    is.numeric(.),
    length(.) == 1,
    is.finite(.),
    . > 0,
    message = "seconds_workers must be a positive number."
  )
}

log_metrics_name <- function(path, name) {
  if_any(
    is.null(path) || path %in% c("/dev/stdout", "/dev/stderr"),
    path,
    file.path(path, name)
  )
}
