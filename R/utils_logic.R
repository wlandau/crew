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

if_any <- function(condition, true, false) {
  if (any(condition)) {
    true
  } else {
    false
  }
}

crew_null <- structure(list(emptyenv()), class = "crew_null")

is_crew_null <- function(x) {
  inherits(x, "crew_null")
}
