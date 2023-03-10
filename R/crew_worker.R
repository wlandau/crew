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
  on.exit(crew_worker_send_token(socket = settings$url, token = token))
  list2env(x = as.list(data), envir = globalenv())
  do.call(what = mirai::server, args = settings)
}

crew_worker_send_token <- function(socket, token) {
  socket <- crew_worker_socket_done(socket)
  connection <- nanonext::socket(protocol = "req", dial = socket)
  on.exit(close(connection))
  crew_wait(~connection$state == "opened", timeout = 5, wait = 0.1)
  nanonext::send(con = connection, data = token)
}

crew_worker_socket_done <- function(socket) {
  out <- gsub(pattern = "^ws://", replacement = "", x = socket)
  out <- gsub(pattern = "/.*$", replacement = "", x = out)
  paste0("ws://", out, "/done")
}
