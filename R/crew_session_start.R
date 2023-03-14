#' @title Start a new crew session.
#' @export
#' @keywords session
#' @description Start a crew session if a session does not already exist.
#' @details A `crew` session reserves a TCP port with a new NNG socket.
#'   `crew` uses this port to monitor the connection statuses of
#'   all workers launched from the current R session across all
#'   controllers and controller groups.
#'   [crew_session_terminate()] closes the NNG socket and frees the port.
#'   Be sure to call [crew_session_terminate()] when you are finished using
#'   `crew` in your R session.
#' @return `NULL` (invisibly).
#' @param host IP address of the machine hosting the session.
#'   Defaults to the local IP address of the session.
#' @param port Positive integer with the port number. Ephemeral
#'   ports between 49152 and 65535 are recommended.
#'   If `port` is `NULL`, a port will be chosen from
#'   `parallelly::freePort()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_session_start()
#' crew_port_get()
#' crew_port_unset()
#' }
crew_session_start <- function(
  host = NULL,
  port = NULL
) {
  true(
    is.null(crew_session_envir$host),
    message = paste(
      "a crew session is already active.",
      "Terminate the current session before starting another."
    )
  )
  host <- as.character(host %|||% local_ip())
  port <- as.integer(port %|||% free_port())
  true(host, is.character(.), !anyNA(.), length(.) == 1L, nzchar(.))
  true(port, is.integer(.), !anyNA(.), length(.) == 1L, . >= 0L, . <= 65535L)
  crew_session_envir$host <- host
  crew_session_envir$port <- port
  crew_session_envir$connection <- connection_listen(
    host = host,
    port = port,
    token = "session"
  )
  invisible()
}

crew_session_host <- function() {
  crew_session_envir$host
}

crew_session_port <- function() {
  crew_session_envir$port
}

crew_session_envir <- new.env(parent = emptyenv())
