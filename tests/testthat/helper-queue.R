callr_force_shutdown <- function(queue) {
  for (handle in queue$get_workers()$handle) {
    if (!is.null(handle)) {
      handle$kill()
    }
  }
}

future_force_shutdown <- function(queue) {
  for (handle in queue$get_workers()$handle) {
    if (!is.null(handle$process)) {
      handle$process$kill()
    }
  }
}
