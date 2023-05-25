#' @title Random name
#' @export
#' @family utilities
#' @description Generate a random string that can be used as a name
#'   for a worker or task.
#' @details The randomness is not reproducible and cannot be set with
#'   e.g. `set.seed()` in R.
#' @return A random character string.
#' @param n Length of the random vector to hash.
#' @examples
#' crew_random_name()
crew_random_name <- function(n = 1L) {
  nanonext::sha1(c(nanonext::random(n = n), as.numeric(Sys.time())))
}
