#' @title Local `crew` launcher options.
#' @export
#' @family options
#' @description Options for the local `crew` launcher.
#' @return A classed list of options for the local launcher.
#' @param log_directory Either `NULL` or a character of length 1
#'   with the file path to a directory to write worker-specific log files
#'   with standard output and standard error messages.
#'   Each log file represents a single *instance* of a running worker,
#'   so there will be more log files
#'   if a given worker starts and terminates a lot. Set to `NULL` to suppress
#'   log files (default).
#' @param log_join Logical of length 1. If `TRUE`, `crew` will write
#'   standard output and standard error to the same log file for
#'   each worker instance. If `FALSE`, then they these two streams
#'   will go to different log files with informative suffixes.
#' @examples
#'   crew_options_local()
crew_options_local <- function(
  log_directory = NULL,
  log_join = TRUE
) {
  out <- structure(
    list(
      log_directory = log_directory,
      log_join = log_join
    ),
    class = c("crew_options_local", "crew_options")
  )
  crew_options_local_validate(out)
  out
}

crew_options_local_validate <- function(options) {
  crew_assert(
    inherits(options, "crew_options_local"),
    message = "options_local object must come from crew_options_local()."
  )
  crew_assert(
    options$log_directory %|||% "x",
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    nzchar(.),
    message = "log_directory must be NULL or a valid directory path."
  )
  crew_assert(
    options$log_join,
    isTRUE(.) || isFALSE(.),
    message = "log_join must be TRUE or FALSE."
  )
}
