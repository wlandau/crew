local_ipv4 <- function() {
  getip::getip(type = "local")
}

tcp_sockets <- function(host, ports) {
  sprintf("tcp://%s:%s", host, ports)
}
