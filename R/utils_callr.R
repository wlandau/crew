callr_session_ready <- function(handle) {
  if (!identical(handle$get_state(), "starting")) {
    return(TRUE)
  }
  result <- handle$read()
  if (is.null(result)) {
    return(FALSE)
  }
  if_any(
    identical(result$code, 201L),
    TRUE,
    crew_error("could not start session worker")
  )
}
