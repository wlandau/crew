crew_assert <- function(condition, message = NULL) {
  if (!all(condition)) {
    default <- paste(deparse(substitute(condition)), "is not true.")
    crew_error(message = message %||% default)
  }
}

crew_error <- function(message) {
  rlang::abort(message = message, class = c("crew_error", "crew"))
}
