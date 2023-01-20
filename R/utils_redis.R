redis_server_exists <- function() {
  all(file.exists(redis_server_path()))
}

redis_server_path <- function() {
  Sys.getenv("CREW_REDIS_SERVER", unset = unname(Sys.which("redis-server")))
}

redis_server_ok <- function() {
  px <- redis_server("--version")
  px$wait()
  px$get_exit_status() == 0L
}

redis_server <- function(...) {
  args <- as.character(c(...))
  processx::process$new(
    command = redis_server_path(),
    args = args,
    stdout = "|",
    stderr = "|",
    cleanup = TRUE,
    supervise = TRUE
  )
}
