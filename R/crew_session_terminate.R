#' @title Close the crew session.
#' @export
#' @keywords session
#' @description Close the crew session.
#' @return `NULL` (invisibly).
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' crew_session_port()
#' crew_session_terminate()
#' }
crew_session_terminate <- function() {
  crew_session_envir$host <- NULL
  crew_session_envir$port <- NULL
  connection <- crew_session_envir$connection
  if (!is.null(connection)) {
    if (connection_opened(connection)) {
      close(connection)
    }
    crew_session_envir$connection <- NULL
  }
  invisible()
}
