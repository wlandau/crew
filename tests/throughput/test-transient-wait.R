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
    x$push(name = name, command = Sys.sleep(10))
    message(paste("push", name))
  }
})
print(time["elapsed"])

# Check the schedule.
testthat::expect_equal(x$schedule$summary()$pushed, 100L)
testthat::expect_equal(x$schedule$summary()$collected, 0L)

# Wait for just one of the tasks.
x$wait(mode = "one")

# Wait a little longer for things to sync up.
Sys.sleep(10)

# Pop just one of the tasks
x$collect()
testthat::expect_equal(x$schedule$summary()$pushed, 96L)
testthat::expect_equal(x$schedule$summary()$collected, 4L)
x$pop(scale = FALSE) # monad data frame
testthat::expect_equal(x$schedule$summary()$pushed, 96L)
testthat::expect_equal(x$schedule$summary()$collected, 3L)

# Clean up.
x$terminate()
