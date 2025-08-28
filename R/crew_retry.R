#' @title Retry code.
#' @export
#' @family utility
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
#' @param max_tries Maximum number of calls to `fun` to try
#'   before giving up.
#' @param error Whether to throw an error on a timeout or max tries.
#' @param message Character of length 1, optional error message
#'   if the wait times out.
#' @param envir Environment to evaluate `fun`.
#' @param assertions `TRUE` to run assertions to check if arguments are
#'   valid, `FALSE` otherwise. `TRUE` is recommended for users.
#' @examples
#' crew_retry(fun = function() TRUE)
crew_retry <- function(
  fun,
  args = list(),
  seconds_interval = 0.25,
  seconds_timeout = 60,
  max_tries = Inf,
  error = TRUE,
  message = character(0),
  envir = parent.frame(),
  assertions = TRUE
) {
  force(envir)
  if (!is.function(fun)) {
    fun <- rlang::as_function(fun)
  }
  if (assertions) {
    crew_assert(is.function(fun))
    crew_assert(is.list(args))
    if (length(args)) {
      crew_assert(names(args), !is.null(.), nzchar(.))
      crew_assert(identical(length(unique(names(args))), length(args)))
    }
    crew_assert(
      seconds_interval,
      is.numeric(.),
      length(.) == 1L,
      !anyNA(.),
      . >= 0
    )
    crew_assert(
      seconds_timeout,
      is.numeric(.),
      length(.) == 1L,
      !anyNA(.),
      . >= 0
    )
    crew_assert(
      max_tries,
      is.numeric(.),
      length(.) == 1L,
      !anyNA(.),
      . >= 0
    )
    crew_assert(error, isTRUE(.) || isFALSE(.))
  }
  milli_timeout <- 1000 * seconds_timeout
  tries <- 0L
  start <- nanonext::mclock()
  while (!isTRUE(do.call(what = fun, args = args, envir = envir))) {
    if ((nanonext::mclock() - start) > milli_timeout) {
      message <- paste(
        "timed out after retrying for",
        seconds_timeout,
        "seconds.",
        message
      )
      if (error) {
        crew_expire(message)
      } else {
        return(invisible())
      }
    }
    tries <- tries + 1L
    if (tries >= max_tries) {
      message <- paste(
        "giving up after",
        max_tries,
        "attempts.",
        message
      )
      if (error) {
        crew_expire(message)
      } else {
        return(invisible())
      }
    }
    nanonext::msleep(time = 1000 * seconds_interval)
  }
  invisible()
}
