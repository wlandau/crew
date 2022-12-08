#' @title Validate arguments
#' @export
#' @family utilities
#' @description Validate input arguments of `crew` functions.
#' @details Used internally and in packages that extend `crew`.
#' @return Nothing (invisibly) if the object is successfully validated,
#'   throws an error of class `"crew_error"`
#'   if at least one of the assertions is not true.
#' @param value Data object to validate.
#' @param ... Assertions to check.
#' @param message Optional character of length 1 with a message to print.
#' @param envir Optional environment to evaluate the assertions.
#' @examples
#' crew_true(letters, length(.) > 0, is.character(.), !anyNA(.))
#' try(crew_true(letters, is.numeric(.), !anyNA(.)))
crew_true <- function(
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


#' @title Throw a crew error.
#' @export
#' @keywords internal
#' @description For internal use only. Not a user-side function.
#'   Do not invoke directly.
#' @return Throw an error of class `c("crew_error", "crew")`.
#' @param message Character of length 1, error message to print.
#' @examples
#' try(crew_error("custom error message"))
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
  withr::local_options(list(rlang_backtrace_on_error = "none"))
  rlang::abort(message = message, class = class, call = crew_empty_envir)
}

crew_message <- function(message) {
  withr::local_options(list(rlang_backtrace_on_error = "none"))
  rlang::inform(message = message, class = c("crew_message", "crew"))
}

#' @title Return `FALSE` on error in a `tryCatch()` statement.
#' @export
#' @keywords internal
#' @description For internal use only. Not a user-side function.
#'   Do not invoke directly.
#' @return `FALSE`
#' @param condition Condition object in the `tryCatch()` statement.
#' @examples
#' tryCatch(stop("error message"), error = crew_condition_false)
crew_condition_false <- function(condition) {
  FALSE
}

#' @title Return `FALSE` on error in a `tryCatch()` statement.
#' @export
#' @keywords internal
#' @description For internal use only. Not a user-side function.
#'   Do not invoke directly.
#' @return `FALSE`
#' @param condition Condition object in the `tryCatch()` statement.
#' @examples
#' tryCatch(stop("error message"), error = crew_condition_message)
crew_condition_message <- function(condition) {
  conditionMessage(condition)
}

crew_empty_envir <- new.env(parent = emptyenv())
