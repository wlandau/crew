local_ipv4 <- function() {
  getip::getip(type = "local")
}

web_sockets <- function(host, port, n = 1L) {
  sprintf("ws://%s:%s/worker%s", host, port, seq_len(n))
}

free_port <- function() {
  parallelly::freePort(
    ports = seq(from = 49152L, to = 65535L, by = 1L),
    default = NA_integer_
  )
}
