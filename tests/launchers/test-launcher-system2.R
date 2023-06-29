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
  tls_enable = TRUE,
  tls_config = NULL,
  seconds_interval = 0.5,
  seconds_timeout = 10,
  seconds_launch = 30,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = 0.1,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE
) {
  client <- crew::crew_client(
    name = name,
    workers = workers,
    host = host,
    port = port,
    tls_enable = tls_enable,
    tls_config = tls_config,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- system2_launcher_class$new(
    name = name,
    seconds_interval = seconds_interval,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection
  )
  controller <- crew::crew_controller(client = client, launcher = launcher)
  controller$validate()
  controller
}

library(crew)
controller <- crew_controller_system2(
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
  ~all(controller$client$summary()$online == FALSE),
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
  ~all(controller$client$summary()$online == FALSE),
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
# Terminate the controller.
controller$terminate()
# Now outside crew, verify that the mirai dispatcher
# and crew workers successfully terminated.
