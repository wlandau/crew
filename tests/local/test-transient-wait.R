crew_test("wait on transient workers", {
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
      x$push(name = name, command = Sys.sleep(10))
    }
  })
  # Wait for just one of the tasks.
  x$wait(mode = "one")
  # Pop just one of the tasks
  testthat::expect_gt(x$resolved(), 0L)
  testthat::expect_gt(x$unresolved(), 0L)
  # Clean up.
  x$terminate()
})
