callr_force_shutdown <- function(queue) {
  for (handle in queue$get_workers()$handle) {
    if (length(handle)) {
      handle$kill()
    }
  }
}
