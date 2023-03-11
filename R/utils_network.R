free_port <- function() {
  parallelly::freePort(
    ports = seq(from = 49152L, to = 65535L, by = 1L),
    default = NA_integer_
  )
}

local_ip <- function() {
  getip::getip(type = "local")
}
