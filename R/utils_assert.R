crew_assert <- function(condition, message = NULL) {
  if (!all(condition)) {
    default <- paste(deparse(substitute(condition)), "is not true.")
    crew_error(message = message %||% default)
  }
}

crew_assert_chr <- function(x, message = NULL) {
  default <- paste(
    x,
    "must be a character string."
  )
  crew_assert(
    condition = is.character(x),
    message = default %||% message
  )
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

crew_assert_file_scalar <- function(x, message = NULL) {
  default <- paste("file", x, "does not exist.")
  crew_assert(
    condition = is.character(x) && length(x) == 1L && all(file.exists(x)),
    message = default %||% message
  )
}

crew_assert_pos_dbl_scalar <- function(x, message = NULL) {
  default <- paste(
    x,
    "must be a positive numeric of length 1."
  )
  crew_assert(
    condition = is.numeric(x) && length(x) == 1L && all(x > 0),
    message = default %||% message
  )
}

crew_assert_nonnegative_dbl_scalar <- function(x, message = NULL) {
  default <- paste(
    x,
    "must be a nonnegative numeric of length 1."
  )
  crew_assert(
    condition = is.numeric(x) && length(x) == 1L && !any(x < 0),
    message = default %||% message
  )
}

crew_assert_lgl_scalar <- function(x, message = NULL) {
  default <- paste(
    x,
    "must be a logical of length 1."
  )
  crew_assert(
    condition = is.logical(x) && length(x) == 1L,
    message = default %||% message
  )
}

crew_assert_named <- function(x, message = NULL) {
  names <- names(x)
  names <- names[nzchar(names)]
  crew_assert(
    length(unique(names)) == length(x),
    paste(
      deparse(substitute(x)),
      "must have nonempty unique names."
    )
  )
}
