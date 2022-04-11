crew_catch_crash <- function(code) {
  tryCatch(
    code,
    error = function(condition) {
      crew_crash(conditionMessage(condition))
    }
  )
}

crew_crash <- function(message = NULL) {
  crew_error(paste("crew worker crashed:", message))
}

crew_error <- function(message = NULL) {
  rlang::abort(
    message = message,
    class = c("crew_error", "crew")
  )
}

crew_expire <- function(message = NULL) {
  rlang::abort(
    message = message,
    class = c("crew_expire", "crew_error", "crew")
  )
}
