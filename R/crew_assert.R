#' @title Crew assertion
#' @export
#' @family utilities
#' @description Assert that a condition is true.
#' @return `NULL` (invisibly). Throws an error if the condition is not true.
#' @param value An object or condition.
#' @param ... Conditions that use the `"."` symbol to refer to the object.
#' @param message Optional message to print on error.
#' @param envir Environment to evaluate the condition.
#' @examples
#' crew_assert(1 < 2)
#' crew_assert("object", !anyNA(.), nzchar(.))
#' tryCatch(
#'   crew_assert(2 < 1),
#'   crew_error = function(condition) message("false")
#' )
crew_assert <- function(
  value = NULL,
  ...,
  message = NULL,
  envir = parent.frame()
) {
  force(envir)
  expr <- match.call(expand.dots = FALSE)$...
  if (!length(expr)) {
    expr <- list(quote(.))
  }
  conditions <- lapply(
    expr,
    function(expr) all(eval(expr, envir = list(. = value), enclos = envir))
  )
  if (!all(unlist(conditions))) {
    chr_expr <- lapply(expr, function(x) sprintf("all(%s)", deparse(x)))
    chr_expr <- paste(unlist(chr_expr), collapse = " && ")
    chr_value <- deparse(substitute(value))
    out <- sprintf("%s is not true on . = %s", chr_expr, chr_value)
    crew_error(message %|||% out)
  }
  invisible()
}

#' @title Deprecate a `crew` feature.
#' @export
#' @family utilities
#' @description Show an informative warning when a `crew` feature is
#'   deprecated.
#' @return `NULL` (invisibly). Throws a warning if a feature is deprecated.
#' @param name Name of the feature (function or argument) to deprecate.
#' @param date Date of deprecation.
#' @param version Package version when deprecation was instated.
#' @param alternative Message about an alternative.
#' @examples
#' suppressWarnings(
#'   crew_deprecate(
#'     name = "auto_scale",
#'     date = "2023-05-18",
#'     version = "0.2.0",
#'     alternative = "use the scale argument of push(), pop(), and wait()."
#'   )
#' )
crew_deprecate <- function(name, date, version, alternative) {
  message <- sprintf(
    "%s was deprecated on %s (crew version %s). Alternative: %s.",
    name,
    date,
    version,
    alternative
  )
  crew_warn(
    message = message,
    class = c("crew_deprecate", "crew_warning", "crew")
  )
}

crew_error <- function(message = NULL) {
  crew_stop(
    message = message,
    class = c("crew_error", "crew")
  )
}

crew_expire <- function(message = NULL) {
  crew_stop(
    message = message,
    class = c("crew_expire", "crew_error", "crew")
  )
}

crew_stop <- function(message, class) {
  old <- getOption("rlang_backtrace_on_error")
  on.exit(options(rlang_backtrace_on_error = old))
  options(rlang_backtrace_on_error = "none")
  rlang::abort(message = message, class = class, call = emptyenv())
}

crew_warn <- function(message, class) {
  old <- getOption("rlang_backtrace_on_error")
  on.exit(options(rlang_backtrace_on_error = old))
  options(rlang_backtrace_on_error = "none")
  rlang::warn(message = message, class = class)
}

crew_message <- function(message) {
  old <- getOption("rlang_backtrace_on_error")
  on.exit(options(rlang_backtrace_on_error = old))
  options(rlang_backtrace_on_error = "none")
  rlang::inform(message = message, class = c("crew_message", "crew"))
}

crew_condition_false <- function(condition) {
  FALSE
}

crew_condition_message <- function(condition) {
  conditionMessage(condition)
}
