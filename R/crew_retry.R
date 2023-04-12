#' @title Retry code.
#' @export
#' @family utilities
#' @description Repeatedly retry a function while it keeps returning `FALSE`
#'   and exit the loop when it returns `TRUE`
#' @return `NULL` (invisibly).
#' @param fun Function that returns `FALSE` to keep waiting
#'   or `TRUE` to stop waiting.
#' @param args A named list of arguments to `fun`.
#' @param seconds_interval Nonnegative numeric of length 1,
#'   number of seconds to wait between calls to `fun`.
#' @param seconds_timeout Nonnegative numeric of length 1,
#'   number of seconds to loop before timing out.
#' @param message Character of length 1, optional error message
#'   if the wait times out.
#' @param envir Environment to evaluate `fun`.
#' @examples
#' crew_retry(fun = function() TRUE)
crew_retry <- function(
  fun,
  args = list(),
  seconds_interval = 1,
  seconds_timeout = 60,
  message = character(0),
  envir = parent.frame()
) {
  force(envir)
  fun <- rlang::as_function(fun)
  true(is.function(fun))
  true(is.list(args))
  if (length(args)) {
    true(names(args), !is.null(.), nzchar(.))
    true(identical(length(unique(names(args))), length(args)))
  }
  true(seconds_interval, is.numeric(.), length(.) == 1L, !anyNA(.), . >= 0)
  true(seconds_timeout, is.numeric(.), length(.) == 1L, !anyNA(.), . >= 0)
  start <- nanonext::mclock()
  milli_timeout <- 1000 * seconds_timeout
  while (!all(do.call(what = fun, args = args, envir = envir))) {
    if ((nanonext::mclock() - start) > milli_timeout) {
      message <- paste(
        "timed out after retrying for",
        seconds_timeout,
        "seconds.",
        message
      )
      crew_expire(message)
    }
    nanonext::msleep(1000 * seconds_interval)
  }
  invisible()
}
