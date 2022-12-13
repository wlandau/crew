crew_sitrep <- function() {
  px <- processx::run(command = "redis-server", args = "--version")
  
  
  redis <- redux::redis_available()
  if (!redis_available) {
    
    redus <- px$status == 0L
  }
}

crew_redis_ok <- function() {
  px <- crew_redis_process("--version")
  px$wait()
  
}

crew_redis_process <- function(...) {
  args <- as.character(c(...))
  processx::process$new(
    command = crew_redis_path(),
    args = args,
    stdout = "|",
    stderr = "|",
    cleanup = TRUE,
    supervise = TRUE
  )
}

crew_redis_exists <- function() {
  nchar(crew_redis_path())
}

crew_redis_path <- function() {
  Sys.getenv("CREW_REDIS", unset = unname(Sys.which("redis-server")))
}
