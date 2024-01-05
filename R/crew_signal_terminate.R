#' @title Get the termination signal.
#' @export
#' @family utility
#' @description Get a supported operating system signal for terminating
#'   a local process.
#' @return An integer of length 1: `tools::SIGTERM` if your platform
#'  supports `SIGTERM`. If not, then `crew_crew_signal_terminate()()` checks
#'  `SIGQUIT`, then `SIGINT`, then `SIGKILL`, and then returns the first
#'  signal it finds that your operating system can use.
crew_signal_terminate <- function() {
  tools::SIGTERM %||NA%
    tools::SIGQUIT %||NA%
    tools::SIGINT %||NA%
    tools::SIGKILL
}
