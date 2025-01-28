crew_test("test auto-scaling of workers", {
  controller <- crew_controller_local(
    seconds_idle = 2L,
    workers = 2L
  )
  controller$start()
  # Push 100 tasks
  for (index in seq_len(100L)) {
    name <- paste0("task_", index)
    controller$push(name = name, command = index, data = list(index = index))
  }
  # Wait for the tasks to complete.
  controller$wait()
  # Do the same for 100 more tasks.
  for (index in (seq_len(100L) + 100L)) {
    name <- paste0("task_", index)
    controller$push(name = name, command = index, data = list(index = index))
  }
  controller$wait()
  # Collect the results.
  results <- NULL
  while (!is.null(out <- controller$pop(scale = FALSE))) {
    if (!is.null(out)) {
      results <- dplyr::bind_rows(results, out)
    }
  }
  # Check the results
  testthat::expect_true(all(sort(unlist(results$result)) == seq_len(200L)))
  controller$terminate()
})
