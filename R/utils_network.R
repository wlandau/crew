tcp_socket <- function(host, port) {
  sprintf("tcp://%s:%s", host, as.character(as.integer(port)))
}

local_ipv4 <- function() {
  getip::getip(type = "local")
}

random_port <- function(lower = 49152L, upper = 65535L) {
  parallelly::freePort(ports = seq.int(from = lower, to = upper, by = 1L))
}
