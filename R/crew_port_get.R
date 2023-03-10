#' @title Get the reserved TCP port for monitoring workers.
#' @export
#' @keywords port
#' @description Get the reserved TCP port for monitoring workers.
#' @return An integer of length 1 with the port number if
#'   the port was set, `NULL` if the port has not been set yet.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' crew_port_set()
#' crew_port_get()
#' crew_port_unset()
#' }
crew_port_get <- function() {
  crew_port_envir$port
}
