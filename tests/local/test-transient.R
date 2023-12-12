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
      x$push(name = name, command = ps::ps_pid())
    }
  })
  message(time["elapsed"])
  # Launch many more tasks.
  time <- system.time({
    for (index in seq_len(n)) {
      name <- paste0("task_", index)
      x$push(name = name, command = ps::ps_pid())
    }
  })
  message(time["elapsed"])
  # Call wait() on the controller to cycle through the rest of the tasks.
  x$wait(mode = "all")
  testthat::expect_equal(x$unresolved(), 0L)
  testthat::expect_equal(length(x$tasks), 200L)
  # All results should now be available.
  results <- list()
  time <- system.time({
    while (length(results) < 2 * n) {
      out <- x$pop()
      if (!is.null(out)) {
        results[[length(results) + 1L]] <- out
      }
    }
  })
  message(time["elapsed"])
  testthat::expect_equal(sum(x$launcher$summary()$launches), 2 * n)
  results <- tibble::as_tibble(do.call(rbind, results))
  results$result <- as.integer(results$result)
  testthat::expect_equal(length(unique(results$result)), 2 * n)
  x$terminate()
})
