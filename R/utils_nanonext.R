connection_dial <- function(host, port, token) {
  socket <- connection_socket(host = host, port = port, token = token)
  connection <- nanonext::socket(protocol = "bus", dial = socket)
  connection_wait_opened(connection)
  connection
}

connection_listen <- function(host, port, token) {
  socket <- connection_socket(host = host, port = port, token = token)
  connection <- nanonext::socket(protocol = "bus", listen = socket)
  connection_wait_opened(connection)
  connection
}

connection_socket <- function(host = host, port = port, token = token) {
  sprintf("ws://%s:%s/%s", host, port, token)
}

connection_closed <- function(connection) {
  identical(as.character(connection$state), "closed")
}

connection_opened <- function(connection) {
  identical(as.character(connection$state), "opened")
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

# TODO: just use .unresolved() when nanonext > 0.8.0.9000 reaches CRAN.
nanonext_unresolved <- if_any(
  exists(x = ".unresolved", envir = getNamespace(name = "nanonext")),
  eval(expr = parse(text = "nanonext::.unresolved")),
  nanonext::unresolved
)
