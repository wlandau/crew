library(crew)
x <- crew_controller_local(
  name = "test",
  tasks_max = 1L,
  workers = 4L
)
x$start()
# Also start htop.

# Launch a lot of tasks on transient workers.
n <- 100
time <- system.time({
  for (index in seq_len(n)) {
    name <- paste0("task_", index)
    x$push(name = name, command = ps::ps_pid())
    message(paste("push", name))
  }
})
message(time["elapsed"])

# Watch htop and wait for workers to scale back down on their own.

# Launch many more tasks.
time <- system.time({
  for (index in seq_len(n)) {
    name <- paste0("task_", index)
    x$push(name = name, command = ps::ps_pid())
    message(paste("push", name))
  }
})
message(time["elapsed"])

# Again, watch htop and wait wait for worker pool can scale back down
# on their own.

# Call wait() on the controller to cycle through the rest of the tasks.
# Watch htop to see it complete.
x$wait(mode = "all")
length(x$queue) # 0
length(x$results) # 200

# All results should now be available.
results <- list()
time <- system.time({
  while (length(results) < 2 * n) {
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
length(unique(results$result)) # Should be 200, one per transient worker.
