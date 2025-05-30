crew_test("custom launcher plugin based on system2()", {
  system2_launcher_class <- R6::R6Class(
    classname = "system2_launcher_class",
    inherit = crew::crew_class_launcher,
    public = list(
      launch_worker = function(call, name, launcher, worker, instance) {
        system2(
          command = file.path(R.home("bin"), "R"),
          args = c("-e", shQuote(call)),
          wait = FALSE,
          stdout = FALSE,
          stderr = FALSE
        )
        invisible()
      }
    )
  )
  crew_controller_system2 <- function(
    name = "system2",
    workers = 1L,
    host = NULL,
    port = NULL,
    tls = crew::crew_tls(),
    serialization = NULL,
    seconds_interval = 0.5,
    seconds_timeout = 10,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE
  ) {
    client <- crew::crew_client(
      host = host,
      port = port,
      tls = tls,
      serialization = serialization,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- system2_launcher_class$new(
      name = name,
      workers = workers,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      tls = tls
    )
    controller <- crew::crew_controller(
      client = client,
      launcher = launcher,
      reset_globals = reset_globals,
      reset_packages = reset_packages,
      reset_options = reset_options,
      garbage_collection = garbage_collection
    )
    controller$validate()
    controller
  }
  controller <- crew_controller_system2(
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
  controller$wait(mode = "all")
  # Do the same for 100 more tasks.
  for (index in (seq_len(100L) + 100L)) {
    name <- paste0("task_", index)
    controller$push(name = name, command = index, data = list(index = index))
  }
  controller$wait(mode = "all")
  # Collect and check the results.
  results <- sort(unlist(controller$collect()$result))
  testthat::expect_true(all(results == seq_len(200L)))
  # Terminate the controller.
  controller$terminate()
})
