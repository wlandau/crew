#' @title Random name
#' @export
#' @family utilities
#' @description Generate a random string that can be used as a name
#'   for a worker or task.
#' @details The randomness is not reproducible and cannot be set with
#'   e.g. `set.seed()` in R.
#' @return A random character string.
#' @param n Number of bytes of information in the random string
#'   hashed to generate the name. Larger `n` is
#'   more likely to generate unique names, but it may be slower to compute.
#' @examples
#' crew_random_name()
crew_random_name <- function(n = 12L) {
  nanonext::random(n = n)
}
