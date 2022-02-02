crew_catch <- function(code) {
  tryCatch(code, error = function(condition) condition)
}

crew_error <- function(message = NULL) {
  rlang::abort(message = message, class = c("crew_error", "crew"))
}

crew_terminate <- function(message = NULL) {
  rlang::abort(message = message, class = c("crew_terminate", "crew"))
}

crew_timeout <- function(message = NULL) {
  rlang::abort(message = message, class = c("crew_timeout", "crew"))
}
