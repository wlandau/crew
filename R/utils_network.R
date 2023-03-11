connection_bus <- function(port, suffix, wait = TRUE) {
  connection <- nanonext::socket(
    protocol = "bus",
    listen = sprintf("ws://%s:%s/%s", local_ip(), port, suffix)
  )
  if (wait) {
    crew_wait(
      ~identical(connection$state, "opened"),
      timeout = 5,
      wait = 0.1
    )
  }
  connection
}

free_port <- function() {
  parallelly::freePort(
    ports = seq(from = 49152L, to = 65535L, by = 1L),
    default = NA_integer_
  )
}

local_ip <- function() {
  getip::getip(type = "local")
}
