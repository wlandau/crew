library(crew)
x <- crew_schedule()
x$start()
null <- new.env(parent = emptyenv())
n_tasks <- 60000L
system.time({
  for (i in seq_len(n_tasks)) {
    x$push(null)
  }
})
system.time(x$collect())
system.time(
  while (x$nonempty()) {
    x$pop()
  }
)
