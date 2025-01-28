`%||%` <- function(x, y) {
  if (length(x) > 0L) {
    x
  } else {
    y
  }
}

`%|||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

`%||NA%` <- function(x, y) {
  if (anyNA(x)) {
    y
  } else {
    x
  }
}

if_any <- function(condition, true, false) {
  if (any(condition)) {
    true
  } else {
    false
  }
}

crew_null <- structure(list("crew_null"), class = "crew_null")

is_crew_null <- function(x) {
  class(x)[1L] == "crew_null" # For performance, we avoid inherits(). # nolint
}
