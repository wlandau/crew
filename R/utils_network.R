tcp_socket <- function(host, port) {
  sprintf("tcp://%s:%s", host, as.character(as.integer(port)))
}

local_ipv4 <- function() {
  getip::getip(type = "local")
}

random_port <- function(lower = 49152L, upper = 65535L) {
  ports <- seq.int(from = lower, to = upper, by = 1L)
  port <- parallelly::freePort(ports = ports)
  as.character(port)
}
