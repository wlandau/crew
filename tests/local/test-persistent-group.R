crew_test("tasks backlog for persistent workers in a controller group", {
  controller <- crew_controller_local(
    name = "test",
    workers = 2L
  )
  x <- crew_controller_group(object = controller)
  x$start()
  n <- 200
  time <- system.time({
    for (index in seq_len(n)) {
      name <- paste0("task_", index)
      x$push(name = name, command = ps::ps_pid())
    }
  })
  message(time["elapsed"])
  results <- list()
  time <- system.time({
    while (length(results) < n) {
      out <- x$pop()
      if (!is.null(out)) {
        results[[length(results) + 1L]] <- out
      }
    }
  })
  message(time["elapsed"])
  x$terminate()
  results <- tibble::as_tibble(do.call(rbind, results))
  results$result <- as.integer(results$result)
  table(results$result) # 200 total tasks should be about evenly distributed.
  testthat::expect_equal(sum(table(results$result)), 200)
})
