connection_dial <- function(port, suffix, ip = local_ip()) {
  socket <- sprintf("ws://%s:%s/%s", ip, port, suffix)
  connection <- nanonext::socket(protocol = "req", dial = socket)
  connection_wait_opened(connection)
  connection
}

connection_listen <- function(port, suffix, ip = local_ip()) {
  socket <- sprintf("ws://%s:%s/%s", ip, port, suffix)
  connection <- nanonext::socket(protocol = "rep", listen = socket)
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
