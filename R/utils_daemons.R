daemons_info <- function(name) {
  out <- mirai::daemons(.compute = name)$daemons
  # Should not happen:
  # nocov start
  if (!daemons_valid(out)) {
    message <- paste(c("invalid daemons:", deparse1(out)), collapse = " ")
    crew_error(message)
  }
  # nocov end
  out
}

daemons_valid <- function(daemons) {
  is.matrix(daemons) && all(dim(daemons) > 0L)
}
