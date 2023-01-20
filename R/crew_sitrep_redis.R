#' @title Check Redis.
#' @export
#' @family sitrep
#' @description Check the Redis installation and configuration
#'   and print messages with any obvious issues.
#' @return Nothing (invisibly). This function prints out messages
#'   to describe the status of the Redis installation and
#'   configuration.
#' @examples
#' crew_sitrep_redis()
crew_sitrep_redis <- function() {
  sitrep_redis_server()
  invisible()
}

sitrep_redis_server <- function() {
  if_any(
    redis_server_path_binary_exists(),
    sitrep_redis_server_path_binary_exists(),
    sitrep_redis_server_not_exist()
  )
}

sitrep_redis_server_path_binary_exists <- function() {
  cli_good("Redis server found: {.path {redis_server_path_binary()}}")
  if_any(
    redis_server_path_binary_ok(),
    cli_good("Redis server ok"),
    sitrep_redis_server_not_ok()
  )
}

sitrep_redis_server_not_exist <- function() {
  cli_bad("Redis server not found: {.path {redis_server_path_binary()}}")
  cli_li("Install Redis server: {.url https://redis.io/download/}")
}

sitrep_redis_server_not_ok <- function() {
  cli_bad("Redis server could not run.")
  cli_li("Help: {.url https://redis.io}")
}
