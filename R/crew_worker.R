#' @title Crew worker.
#' @export
#' @keywords internal
#' @family utilities
#' @description Launch a `crew` worker which runs a `mirai` server.
#' @return `NULL` (invisibly)
#' @param settings Named list of arguments to `mirai::server()`.
#' @param token Character of length 1 to identify the instance of the
#'   process connected to the socket.
crew_worker <- function(settings, token) {
  con <- crew_worker_connection(socket = settings$url, token = token)
  on.exit(close(con))
  do.call(what = mirai::server, args = settings)
}

crew_worker_connection <- function(socket, token) {
  socket <- crew_worker_socket(socket = socket, token = token)
  connection <- nanonext::socket(protocol = "req", dial = socket)
  connection_wait(connection)
  connection
}

crew_worker_socket <- function(socket, token) {
  out <- gsub(pattern = "^ws://", replacement = "", x = socket)
  out <- gsub(pattern = "/.*$", replacement = "", x = out)
  paste0("ws://", out, "/", token)
}
