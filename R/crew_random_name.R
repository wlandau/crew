#' @title Random name
#' @export
#' @family utilities
#' @description Generate a random string that can be used as a name
#'   for a worker or task.
#' @details The randomness is not reproducible and cannot be set with
#'   e.g. `set.seed()` in R.
#' @return A random character string.
#' @examples
#' crew_random_name()
crew_random_name <- function() {
  nanonext::sha1(nanonext::random(n = 100L))
}
