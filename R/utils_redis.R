redis_server_ok_binary <- function() {
  px <- redis_server_process("--version")
  px$wait()
  px$get_exit_status() == 0L
}

redis_server_ok_conf <- function() {
  px <- redis_server()
  
  
  px$get_exit_status() == 0L
}

redis_server <- function() {
  redis_server_process(redis_server_path_conf())
}

redis_server_process <- function(...) {
  args <- as.character(c(...))
  processx::process$new(
    command = redis_server_path_binary(),
    args = args,
    stdout = "|",
    stderr = "|",
    cleanup = TRUE,
    supervise = TRUE
  )
}

redis_server_exists_binary <- function() {
  all(file.exists(redis_server_path_binary()))
}

redis_server_exists_conf <- function() {
  all(file.exists(redis_server_path_binary()))
}

redis_server_path_binary <- function() {
  Sys.getenv("CREW_REDIS_SERVER", unset = unname(Sys.which("redis-server")))
}

redis_server_path_conf <- function() {
  Sys.getenv("CREW_REDIS_CONF", unset = "")
}

redis_server_path_host <- function() {
  Sys.getenv("CREW_REDIS_HOST", unset = "127.0.0.1"))
}

redis_server_path_port <- function() {
  Sys.getenv("CREW_REDIS_PORT", unset = "6379"))
}

redis_server_path_password <- function() {
  Sys.getenv("CREW_REDIS_PASSWORD", unset = ""))
}
