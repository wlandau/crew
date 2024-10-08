#' @title Validate options.
#' @export
#' @family options
#' @description Validate a `crew` options list.
#' @return `NULL` (invisibly) on success, throws an error if there is
#'   something wrong with the options list.
crew_options_validate <- function(options) {
  UseMethod("crew_options_validate")
}
