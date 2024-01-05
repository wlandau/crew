#' @title Get the termination signal.
#' @export
#' @family utility
#' @description Get a supported operating system signal for terminating
#'   a local process.
#' @return An integer of length 1: `tools::SIGTERM` if your platform
#'  supports `SIGTERM`. If not, then `crew_crew_terminate_signal()()` checks
#'  `SIGQUIT`, then `SIGINT`, then `SIGKILL`, and then returns the first
#'  signal it finds that your operating system can use.
#' @examples
#' crew_terminate_signal()
crew_terminate_signal <- function() {
  out <- tools::SIGTERM %||NA%
    tools::SIGQUIT %||NA%
    tools::SIGINT %||NA%
    tools::SIGKILL
  crew_assert(
    out,
    is.finite(.),
    message = paste(
      "crew::crew_terminate_signal() could not find a termination signal",
      "that your operating system supports. Please file a bug report at",
      "https://github.com/wlandau/crew/issues/new and describe  the details",
      "of your system."
    )
  )
  out
}
