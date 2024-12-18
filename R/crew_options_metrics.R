#' @title Options for logging resource usage metrics.
#' @export
#' @family options
#' @description [crew_options_metrics()] configures the
#'   `crew` to record resource usage metrics (such as CPU and memory usage)
#'   for each running worker.
#'   To be activate resource usage logging,
#'   the `autometric` R package version 0.1.0 or higher
#'   must be installed.
#'
#'   Logging happens in the background (through a detached POSIX)
#'   so as not to disrupt
#'   the R session. On Unix-like systems, [crew_options_metrics()]
#'   can specify `/dev/stdout` or `/dev/stderr` as the log files, which will
#'   redirect output to existing logs you are already using.
#'   [autometric::log_read()] and [autometric::log_plot()] can read and
#'   visualize resource usage data from multiple log files, even
#'   if those files are mixed with other messages.
#' @return A classed list of options for logging resource usage metrics.
#' @param path Where to write resource metric log entries for workers.
#'   `path = NULL` disables logging. `path` equal to `"/dev/stdout"`
#'   (or `"/dev/stderr"`) sends log messages to the standard output
#'   (or standard error) streams, which is recommended on Unix-like systems
#'   because then output will go to the existing log files already
#'   configured for the
#'   controller, e.g. through [crew_options_local()] in the case of
#'   [crew_controller_local()]. If `path` is not `NULL`, `"/dev/stdout"`, or
#'   `"/dev/stderr"`, it should be a directory path,
#'   in which case each worker instance will write to a new file in that
#'   directory.
#'
#'   After running enough tasks in `crew`, you can call
#'   `autometric::log_read(path)` to read all the data from all the log
#'   files in the files or directories at `path`,
#'   even if the logs files are mixed with other kinds of messages.
#'   Pass that data into [autometric::log_plot()] to visualize it.
#' @param seconds_interval Positive number, seconds between resource metric
#'   log entries written to `path`.
#' @examples
#'   crew_options_metrics()
crew_options_metrics <- function(path = NULL, seconds_interval = 5) {
  out <- structure(
    list(path = path, seconds_interval = seconds_interval),
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
    options$path %|||% "x",
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    nzchar(.),
    message = paste(
      "path must be NULL, \"/dev/stdout\", ",
      "\"/dev/stderr\", or a valid directory path."
    )
  )
  on_windows <- identical(unname(tolower(Sys.info()[["sysname"]])), "windows")
  streams <- file.path("/dev", c("stdout", "stderr"))
  crew_assert(
    !(on_windows && any(options$path %in% streams)),
    message = paste(
      "path cannot be \"/dev/stdout\" or \"/dev/stderr\" on Windows."
    )
  )
  crew_assert(
    options$seconds_interval,
    is.numeric(.),
    length(.) == 1,
    is.finite(.),
    . > 0,
    message = "seconds_interval must be a positive number."
  )
}
