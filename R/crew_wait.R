#' @title Wait for an event.
#' @export
#' @keywords internal
#' @family utilities
#' @description Repeatedly run a function while it keeps returning `FALSE`
#'   and exit the loop when it returns `TRUE`
#' @return `NULL` (invisibly).
#' @param fun Function that returns `FALSE` to keep waiting
#'   or `TRUE` to stop waiting.
#' @param args A named list of arguments to `fun`.
#' @param timeout Nonnegative numeric of length 1,
#'   number of seconds to loop before timing out.
#' @param wait Nonnegative numeric of length 1,
#'   number of seconds to wait between calls to `fun`.
#' @param message Character of length 1, optional error message
#'   if the wait times out.
#' @examples
#' crew_wait(fun = function() TRUE)
crew_wait <- function(
  fun,
  args = list(),
  timeout = 60,
  wait = 1,
  message = character(0)
) {
  fun <- rlang::as_function(fun)
  true(is.function(fun))
  true(is.list(args))
  if (length(args)) {
    true(names(args), !is.null(.), nzchar(.))
    true(identical(length(unique(names(args))), length(args)))
  }
  true(timeout, is.numeric(.), length(.) == 1L, !anyNA(.), . >= 0)
  true(wait, is.numeric(.), length(.) == 1L, !anyNA(.), . >= 0)
  start <- as.numeric(proc.time()["elapsed"])
  while (!all(do.call(what = fun, args = args))) {
    if (as.numeric(proc.time()["elapsed"]) - start > timeout) {
      message <- paste("timed out after waiting", timeout, "seconds.", message)
      crew_expire(message)
    }
    Sys.sleep(wait)
  }
  invisible()
}
