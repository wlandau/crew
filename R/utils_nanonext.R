connection_dial <- function(host, port, token) {
  socket <- sprintf("ws://%s:%s/%s", host, port, token)
  connection <- nanonext::socket(protocol = "bus", dial = socket)
  connection_wait_opened(connection)
  connection
}

connection_listen <- function(host, port, token) {
  socket <- sprintf("ws://%s:%s/%s", host, port, token)
  connection <- nanonext::socket(protocol = "bus", listen = socket)
  connection_wait_opened(connection)
  connection
}

connection_closed <- function(connection) {
  identical(as.character(connection$state), "closed")
}

connection_opened <- function(connection) {
  identical(as.character(connection$state), "opened")
}

connection_wait_closed <- function(connection) {
  crew_wait(
    ~connection_closed(connection),
    timeout = 5,
    wait = 0.001
  )
}

connection_wait_opened <- function(connection) {
  crew_wait(
    ~connection_opened(connection),
    timeout = 5,
    wait = 0.001
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
