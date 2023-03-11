#' @title Reserve a port for monitoring workers.
#' @export
#' @keywords port
#' @description Reserve a TCP port to monitor updates from workers.
#' @details When you set the port, it is reserved with an NNG socket.
#'   [crew_port_unset()] closes the socket and frees the port.
#'   Be sure to call [crew_port_unset()] when you are finished using
#'   `crew` in your R session.
#' @return `NULL` (invisibly).
#' @param port Positive integer with the port number. Ephemeral
#'   ports between 49152 and 65535 are recommended.
#'   If `port` is `NULL`, a port will be chosen from
#'   `parallelly::freePort()`.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_port_set()
#' crew_port_get()
#' crew_port_unset()
#' }
crew_port_set <- function(port = NULL) {
  port <- port %|||% free_port()
  true(port, is.integer(.), !anyNA(.), length(.) == 1L, . >= 0L, . <= 65535L)
  crew_port_envir$port <- port
  crew_port_envir$socket <- connection_bus(
    port = port,
    suffix = "crew",
    wait = TRUE
  )
  invisible()
}

crew_port_envir <- new.env(parent = emptyenv())
