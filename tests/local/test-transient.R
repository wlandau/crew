crew_test("backlog of tasks for transient workers", {
  x <- crew_controller_local(
    name = "test",
    tasks_max = 1L,
    workers = 4L
  )
  x$start()
  n <- 100
  time <- system.time({
    for (index in seq_len(n)) {
      name <- paste0("task_", index)
      x$push(name = name, command = Sys.getenv("CREW_WORKER"))
    }
  })
  message(time["elapsed"])
  # Launch many more tasks.
  time <- system.time({
    for (index in seq_len(n)) {
      name <- paste0("task_", index + 1000L)
      x$push(name = name, command = Sys.getenv("CREW_WORKER"))
    }
  })
  message(time["elapsed"])
  # Call wait() on the controller to cycle through the rest of the tasks.
  message(system.time(x$wait(mode = "all"))["elapsed"])
  testthat::expect_equal(x$unresolved(), 0L)
  testthat::expect_equal(length(x$tasks), 200L)
  # All results should now be available.
  results <- unlist(x$collect()$result)
  testthat::expect_equal(length(unique(results)), 2 * n)
  x$terminate()
})
