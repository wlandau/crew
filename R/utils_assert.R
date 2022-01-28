crew_assert <- function(condition) {
  if (!all(condition)) {
    crew_error(message = paste(deparse(substitute(condition)), "is not true."))
  }
}

crew_error <- function(message) {
  rlang::abort(message = message, class = c("crew_error", "crew"))
}
