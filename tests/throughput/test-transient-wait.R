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
    x$push(name = name, command = Sys.sleep(10))
    message(paste("push", name))
  }
})
print(time["elapsed"])

# Check the queue and results lists.
length(x$queue) # 100
length(x$results) # 0

# Wait for just one of the tasks.
x$wait(mode = "one")

# Pop just one of the tasks
length(x$queue) # 96
length(x$results) # 4
x$pop(scale = FALSE) # monad data frame
length(x$queue) # 96
length(x$results) # 3

# Clean up.
View(x$summary())
x$terminate()
crew_session_terminate()
