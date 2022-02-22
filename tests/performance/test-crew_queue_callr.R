proffer::pprof({
library(crew)
x <- crew:::crew_queue_callr$new(workers = 4)
for (index in 1:1000) {
  x$push(
    fun = function(x) x,
    args = list(x = index),
    task = as.character(index)
  )
  out <- x$pop()
  if (!is.null(out)) {
    print(out$result$result)
  }
}
not_done <- function(x) {
  nrow(x$get_tasks()) ||
    nrow(x$get_results()) ||
    any(!x$get_workers()$free)
}
for (index in 1:1000) {
  while (not_done(x)) {
    out <- x$pop()
    if (!is.null(out)) {
      print(out$result$result)
    }
  }
}
x$shutdown()
})
