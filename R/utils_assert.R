crew_assert <- function(condition, message = NULL) {
  if (!all(condition)) {
    default <- paste(deparse(substitute(condition)), "is not true.")
    crew_error(message = message %||% default)
  }
}

crew_assert_chr_scalar <- function(x, message = NULL) {
  default <- paste(
    x,
    "must be a nonempty character string of length 1."
  )
  crew_assert(
    condition = is.character(x) && length(x) == 1L && all(nzchar(x)),
    message = default %||% message
  )
}

crew_error <- function(message) {
  rlang::abort(message = message, class = c("crew_error", "crew"))
}
