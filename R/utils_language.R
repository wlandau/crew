deparse_safe <- function(expr, collapse = "\n", backtick = TRUE) {
  out <- deparse_direct(
    expr,
    control = deparse_control_custom,
    backtick = backtick
  )
  if (length(out) > 1L) {
    out <- paste(out, collapse = collapse)
  }
  out
}

deparse_control_custom <- .deparseOpts(c("keepNA", "keepInteger"))

deparse_direct <- function(...) {
  produce_deparse_direct()(...)
}

produce_deparse_direct <- function() {
  .deparseOpts <- identity
  environment(deparse) <- environment()
  deparse
}
