proffer::pprof({
library(crew)
x <- crew:::crew_sync$new()
x$add_workers(4)
for (i in 1:1000) {
  x$push(
    fun = function(x) x,
    args = list(x = i),
    task = as.character(i)
  )
  out <- x$pop()
  if (!is.null(out)) {
    print(out$result$result)
  }
}
for (i in 1:1000) {
  while (nrow(x$get_tasks()) || nrow(x$get_results()) || any(!x$get_workers()$free)) {
    out <- x$pop()
    if (!is.null(out)) {
      print(out$result$result)
    }
  }
}
x$shutdown()
})
