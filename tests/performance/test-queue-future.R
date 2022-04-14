x <- queue_future$new(workers = 4, processes = 4)
n <- 10#2e2
submitted <- integer(0)
done <- integer(0)
for (index in seq_len(n)) {
  x$push(
    fun = function(x) x,
    args = list(x = index),
    task = sprintf("job_%s", index)
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
x$shutdown()
