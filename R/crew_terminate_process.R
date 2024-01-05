#' @title Manually terminate a local process.
#' @export
#' @family utility
#' @description Manually terminate a local process.
#' @return `NULL` (invisibly).
#' @param pid Integer of length 1, process ID to terminate.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' process <- processx::process$new("sleep", "60")
#' process$is_alive()
#' crew_terminate_process(pid = process$get_pid())
#' process$is_alive()
#' }
crew_terminate_process <- function(pid) {
  crew_assert(
    pid,
    is.numeric(.),
    length(.) == 1L,
    is.finite(.),
    . >= 0L,
    message = "pid must be a nonnegative finite numeric of length 1."
  )
  tools::pskill(
    pid = as.integer(pid),
    signal = crew_terminate_signal()
  )
}
