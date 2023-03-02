tcp_sockets <- function(host, ports) {
  sprintf("tcp://%s:%s", host, as.character(as.integer(ports)))
}

local_ipv4 <- function() {
  getip::getip(type = "local")
}

random_ports <- function(n = 1L, lower = 49152L, upper = 65535L) {
  ports <- seq.int(from = lower, to = upper, by = 1L)
  out <- integer(0)
  for (index in seq_len(n)) {
    port <- parallelly::freePort(ports = ports)
    out <- c(out, port)
    ports <- setdiff(ports, port)
  }
  sort(out)
}
