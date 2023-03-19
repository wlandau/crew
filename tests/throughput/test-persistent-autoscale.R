library(crew)
crew_session_start()
x <- crew_controller_callr(
  seconds_idle = 2L,
  workers = 2L
)
x$start()
for (index in seq_len(100L)) {
  name <- paste0("task_", index)
  x$push(name = name, command = index, data = list(index = index))
  message(paste("push", name))
}
x$wait()
Sys.sleep(3)
for (index in (seq_len(100L) + 100L)) {
  name <- paste0("task_", index)
  x$push(name = name, command = index, data = list(index = index))
  message(paste("push", name))
}
x$wait()
results <- NULL
while (!is.null(out <- x$pop(scale = FALSE))) {
  if (!is.null(out)) {
    results <- dplyr::bind_rows(results, out)
  }
}
all(sort(unlist(results$result)) == seq_len(200L))
#> [1] TRUE
length(unique(results$socket_session))
#> [1] 4
x$summary()$worker_launches
#> [1] 2 2
View(x$summary(-contains(c("controller", "seconds"))))
x$terminate()
