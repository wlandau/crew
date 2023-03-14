#' @title Crew worker.
#' @export
#' @family utilities
#' @description Launch a `crew` worker which runs a `mirai` server.
#' @return `NULL` (invisibly)
#' @param settings Named list of arguments to `mirai::server()`.
#' @param host IP address of the host to connect to.
#' @param port TCP port to register a successful connection to the host.
#' @param token Character of length 1 to identify the instance of the
#'   process connected to the socket.
crew_worker <- function(settings, host, port, token) {
  connection <- connection_dial(host = host, port = port, token = token)
  on.exit(close(connection))
  do.call(what = mirai::server, args = settings)
}
