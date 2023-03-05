is_launcher <- function(x) {
  UseMethod("is_launcher")
}

#' @export
#' @keywords internal
is_launcher.default <- function(x) {
  FALSE
}
