is_controller <- function(x) {
  UseMethod("is_controller")
}

#' @export
#' @keywords internal
is_controller.default <- function(x) {
  FALSE
}

is_router <- function(x) {
  UseMethod("is_router")
}

#' @export
#' @keywords internal
is_router.default <- function(x) {
  FALSE
}

is_launcher <- function(x) {
  UseMethod("is_launcher")
}

#' @export
#' @keywords internal
is_launcher.default <- function(x) {
  FALSE
}
