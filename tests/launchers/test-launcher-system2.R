system2_launcher_class <- R6::R6Class(
  classname = "system2_launcher_class",
  inherit = crew::crew_class_launcher,
  public = list(
    launch_worker = function(socket, host, port, token, name) {
      call <- self$call(socket, host, port, token, name)
      system2(
        command = "R",
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
    seconds_launch = 30,
    seconds_interval = 0.001,
    seconds_timeout = 5,
    seconds_idle = Inf,
    seconds_wall = Inf,
    seconds_exit = 0.1,
    tasks_max = Inf,
    tasks_timers = 0L,
    async_dial = TRUE,
    cleanup = FALSE,
    auto_scale = "demand"
) {
  router <- crew::crew_router(
    name = name,
    workers = workers,
    host = host,
    port = port,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  launcher <- system2_launcher_class$new(
    name = name,
    seconds_launch = seconds_launch,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    seconds_exit = seconds_exit,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    async_dial = async_dial,
    cleanup = cleanup
  )
  controller <- crew::crew_controller(
    router = router,
    launcher = launcher,
    auto_scale = auto_scale
  )
  controller$validate()
  controller
}

library(crew)
crew_session_start()
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
crew_wait(
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
crew_wait(
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
all(sort(unlist(results$result)) == seq_len(200L))
#> [1] TRUE
length(unique(results$socket_session))
#> [1] 4
# View worker and task summaries.
View(controller$summary())
# Terminate the controller.
controller$terminate()
# Now outside crew, verify that the mirai dispatcher
# and crew workers successfully terminated.
