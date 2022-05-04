# Small test
start <- unname(proc.time()["elapsed"])
future::plan(
  future.batchtools::batchtools_sge,
  template = file.path(getwd(), "sge.tmpl")
)
store <- store_local$new(dir_root = "store", timeout = Inf)
x <- queue_future$new(
  workers = 1,
  store = store,
  subqueue = queue_session$new(workers = 1)
)
x$push(fun = function() "x")
x$update()
x$shutdown()
store$destroy()

# Longer test
start <- unname(proc.time()["elapsed"])
future::plan(
  future.batchtools::batchtools_sge,
  template = file.path(getwd(), "sge.tmpl")
)
store <- store_local$new(dir_root = "store", timeout = Inf)
x <- queue_future$new(
  workers = 20,
  store = store,
  subqueue = queue_session$new(workers = 6)
)
n <- 200
submitted <- integer(0)
done <- integer(0)
for (index in seq_len(n)) {
  x$push(
    fun = function(x) {
      x
    },
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
crew_assert(all(sort(done) == sort(seq_len(n))))
x$shutdown()
store$destroy()
print(sprintf("%s min", (unname(proc.time()["elapsed"]) - start) / 60))
