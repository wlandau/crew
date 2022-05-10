system.time({
rstudioapi::restartSession()
la()
store <- crew_store_local$new(timeout = 5, wait = 0.1, root = "~/Desktop/store")
x <- crew_queue_bg$new(workers = 1, store = store, timeout = 5)
n <- 2e2
submitted <- integer(0)
done <- integer(0)
for (index in seq_len(n)) {
  x$push(
    fun = function(x) x,
    args = list(x = index),
    task = as.character(index)
  )
  submitted <- c(submitted, index)
  out <- x$pop()
  if (!is.null(out)) {
    out <- out$result$result
    print(out)
    done <- c(done, out)
  }
}
not_done <- function(x) {
  nrow(x$get_tasks()) ||
    nrow(x$get_results()) ||
    any(!x$get_workers()$free)
}
for (index in seq_len(n)) {
  while (not_done(x)) {
    out <- x$pop()
    if (!is.null(out)) {
      out <- out$result$result
      print(out)
      done <- c(done, out)
    }
  }
}
stopifnot(all(sort(done) == seq_len(n)))
x$shutdown()
})
