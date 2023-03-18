library(crew)
crew_session_start()
x <- crew_controller_callr(
  name = "test",
  seconds_poll_high = 0.001,
  seconds_poll_low = 0.01,
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

# Check that the worker pool can scale back down. Workers should exit.
crew_wait(
  fun = ~identical(x$launcher$active(), character(0L)),
  seconds_interval = 0.001,
  seconds_timeout = 10
)

# Launch many more tasks.
time <- system.time({
  for (index in seq_len(n)) {
    name <- paste0("task_", index)
    x$push(name = name, command = ps::ps_pid())
    message(paste("push", name))
  }
})
message(time["elapsed"])

# Again, wait for worker pool can scale back down. Workers should exit.
crew_wait(
  fun = ~identical(x$launcher$active(), character(0L)),
  seconds_interval = 0.001,
  seconds_timeout = 10
)

# Call wait() on the controller to cycle throught the rest of the tasks.
# Watch htop to see it complete.
x$wait(mode = "all")
length(x$queue) # 0
length(x$results) # 200

# Only the first 8 tasks should have run so far. Keep calling pop()
# until the rest of the tasks complete.
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
crew_session_terminate()
