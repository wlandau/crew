crew_test("test handling of launch failures", {
  envir <- new.env(parent = emptyenv())
  envir$result <- character(0L)
  system2_launcher_class <- R6::R6Class(
    classname = "system2_launcher_class",
    inherit = crew::crew_class_launcher,
    public = list(
      launch_worker = function(call, name) {
        total <- self$launches$total[nrow(self$launches)]
        if (length(total) > 0L && total %% 2L) {
          system2(
            command = file.path(R.home("bin"), "R"),
            args = c("-e", shQuote(call)),
            wait = FALSE,
            stdout = FALSE,
            stderr = FALSE
          )
          envir$result <- c(envir$result, "success")
        } else {
          envir$result <- c(envir$result, "failure")
        }
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
    seconds_launch = 1,
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
    tasks_max = 1L,
    workers = 1L,
    seconds_launch = 3
  )
  on.exit(controller$terminate())
  n_tasks <- 7L
  cli::cli_progress_bar(total = 7L)
  for (index in seq_len(n_tasks)) {
    controller$push(index, data = list(index = index))
    controller$wait()
    cli::cli_progress_update()
  }
  cli::cli_progress_done()
  expect_equal(envir$result, rep(c("failure", "success"), times = 7L))
  results <- controller$collect()
  expect_equal(sort(as.integer(results$result)), seq_len(n_tasks))
  expect_equal(controller$launcher$failed, 7L)
  status <- controller$client$status()
  expect_equal(as.integer(status["connections"]), 0L)
  expect_equal(as.integer(status["awaiting"]), 0L)
  expect_equal(as.integer(status["executing"]), 0L)
  expect_equal(as.integer(status["completed"]), 7L)
  expect_equal(as.integer(status["cumulative"]), 7L)
})
