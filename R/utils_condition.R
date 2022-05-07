crew_catch_crash <- function(code) {
  tryCatch(
    code,
    error = function(condition) {
      crew_crash(conditionMessage(condition))
    }
  )
}

crew_crash <- function(message = NULL) {
  crew_error(paste("crew worker crashed:", message))
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
  rlang::abort(
    message = message,
    class = c("crew_error", "crew")
  )
}

crew_expire <- function(message = NULL) {
  rlang::abort(
    message = message,
    class = c("crew_expire", "crew_error", "crew")
  )
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

#' @title Return the condition message in a `tryCatch()` statement.
#' @export
#' @keywords internal
#' @description For internal use only. Not a user-side function.
#'   Do not invoke directly.
#' @return Character string with the condition message.
#' @param condition Condition object in the `tryCatch()` statement.
#' @examples
#' tryCatch(stop("error message"), error = crew_condition_message)
crew_condition_message <- function(condition) {
  conditionMessage(condition)
}
