`%||%` <- function(x, y) {
  if (length(x) <= 0L) {
    y
  }
  else {
    x
  }
}

if_any <- function(condition, true, false) {
  if (any(condition)) {
    true
  }
  else {
    false
  }
}
