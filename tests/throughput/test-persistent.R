library(crew)
crew_session_start()
x <- crew_controller_callr(
  name = "test",
  workers = 2L
)
x$start()
n <- 200
time <- system.time({
  for (index in seq_len(n)) {
    name <- paste0("task_", index)
    x$push(name = name, command = ps::ps_pid())
    message(paste("push", name))
  }
})
message(time["elapsed"])
results <- list()
time <- system.time({
  while (length(results) < n) {
    out <- x$pop()
    if (!is.null(out)) {
      results[[length(results) + 1L]] <- out
      message(paste("done", out$name, out$result[[1]]))
    }
  }
})
message(time["elapsed"])
View(x$summary())
x$terminate()
results <- tibble::as_tibble(do.call(rbind, results))
results$result <- as.integer(results$result)
table(results$result) # 200 total tasks should be about evenly distributed.
crew_session_terminate()
