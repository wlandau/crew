#' @title Crew worker.
#' @export
#' @keywords internal
#' @family utilities
#' @description Launch a `crew` worker which runs a `mirai` server.
#' @return `NULL` (invisibly)
#' @param settings Named list of arguments to `mirai::server()`.
#' @param token Character of length 1 to identify the instance of the
#'   process connected to the socket.
#' @param data Named list of R objects to assign to the environment
#'   of the worker process.
crew_worker <- function(settings, token, data = NULL) {
  con <- crew_worker_connection(socket = settings$url, token = token)
  on.exit(close(con))
  list2env(x = as.list(data), envir = globalenv())
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
