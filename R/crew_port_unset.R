#' @title Unset and release the TCP port for monitoring workers.
#' @export
#' @keywords port
#' @description Unset and release the TCP port for monitoring workers.
#' @details This function closes the NNG socket that reserved the port,
#'   freeing it for other programs to use. If you previously set a port with
#'   [crew_port_set()], always call `crew_port_unset()` when you
#'   are done using `crew` for the current R session.
#' @return `NULL` (invisibly).
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_port_set()
#' crew_port_get()
#' crew_port_unset()
#' }
crew_port_unset <- function() {
  crew_port_envir$port <- NULL
  if (!is.null(crew_port_envir$connection)) {
    close(crew_port_envir$connection)
    crew_port_envir$connection <- NULL
  }
  invisible()
}
