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