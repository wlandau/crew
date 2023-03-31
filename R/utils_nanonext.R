connection_dial <- function(host, port, token) {
  socket <- connection_socket(host = host, port = port, token = token)
  nanonext::socket(protocol = "bus", dial = socket)
}

connection_listen <- function(host, port, token) {
  socket <- connection_socket(host = host, port = port, token = token)
  nanonext::socket(protocol = "bus", listen = socket)
}

connection_socket <- function(host = host, port = port, token = token) {
  sprintf("ws://%s:%s/%s", host, port, token)
}

connection_closed <- function(connection) {
  as.character(connection$state %||% "closed") == "closed"
}

connection_opened <- function(connection) {
  as.character(connection$state %||% "closed") == "opened"
}

connection_wait_closed <- function(
  connection,
  seconds_interval = 0.001,
  seconds_timeout = 5
) {
  crew_wait(
    ~connection_closed(connection),
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
}

connection_wait_opened <- function(
  connection,
  seconds_interval = 0.001,
  seconds_timeout = 5
) {
  crew_wait(
    ~connection_opened(connection),
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
}

dialer_connected <- function(listener) {
  connection_opened(listener) &&
    nanonext::stat(listener$listener[[1]], "pipes") > 0L
}

dialer_discovered <- function(listener) {
  connection_opened(listener) &&
    nanonext::stat(listener$listener[[1]], "accept") > 0L
}

dialer_not_discovered <- function(listener) {
  connection_closed(listener) ||
    nanonext::stat(listener$listener[[1]], "accept") < 1L
}

listener_connected <- function(dialer) {
  connection_opened(dialer) &&
    nanonext::stat(dialer$dialer[[1]], "pipes") > 0L
}
