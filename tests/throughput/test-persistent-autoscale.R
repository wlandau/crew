library(crew)
controller <- crew_controller_local(
  seconds_idle = 2L,
  workers = 2L
)
controller$start()
# Push 100 tasks
for (index in seq_len(100L)) {
  name <- paste0("task_", index)
  controller$push(name = name, command = index, data = list(index = index))
  message(paste("push", name))
}
# Wait for the tasks to complete.
controller$wait()
# Wait for the workers to idle out and exit on their own.
crew_retry(
  ~all(controller$summary()$worker_connected == FALSE),
  seconds_interval = 1,
  seconds_timeout = 60
)
# Do the same for 100 more tasks.
for (index in (seq_len(100L) + 100L)) {
  name <- paste0("task_", index)
  controller$push(name = name, command = index, data = list(index = index))
  message(paste("push", name))
}
controller$wait()
crew_retry(
  ~all(controller$summary()$worker_connected == FALSE),
  seconds_interval = 1,
  seconds_timeout = 60
)
# Collect the results.
results <- NULL
while (!is.null(out <- controller$pop(scale = FALSE))) {
  if (!is.null(out)) {
    results <- dplyr::bind_rows(results, out)
  }
}
# Check the results
testthat::expect_true(all(sort(unlist(results$result)) == seq_len(200L)))
testthat::expect_equal(length(unique(results$instance)), 4L)
controller$terminate()
# Now outside crew, verify that the mirai dispatcher
# and crew workers successfully terminated.
