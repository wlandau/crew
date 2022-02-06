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

#' @title Wait for an event.
#' @export
#' @family utilities
#' @description Repeatedly run a function while it keeps returning `FALSE`
#'   and exit the loop when it returns `TRUE`
#' @return `NULL` (invisibly).
#' @param fun Function that returns `FALSE` to keep waiting
#'   `TRUE` to stop waiting.
#' @param args A named list of arguments to `fun`.
#' @param timeout Nonnegative numeric of length 1,
#'   number of seconds to loop before timing out.
#' @param wait Nonnegative numeric of length 1,
#'   number of seconds to wait between calls to `fun`.
crew_wait <- function(fun, args = list(), timeout = 60, wait = 1) {
  crew_assert(is.function(fun))
  crew_assert(is.list(args))
  crew_assert_named(args)
  crew_assert_nonnegative_dbl_scalar(timeout)
  crew_assert_nonnegative_dbl_scalar(wait)
  start <- as.numeric(proc.time()["elapsed"])
  while (!all(do.call(what = fun, args = args))) {
    if (as.numeric(proc.time()["elapsed"]) - start > timeout) {
      crew_timeout(sprintf("timed out after waiting %s seconds.", timeout))
    }
    Sys.sleep(wait)
  }
  invisible()
}
