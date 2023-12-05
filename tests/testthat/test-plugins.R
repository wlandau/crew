crew_test("custom launcher", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("processx")
  if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
    skip_on_os("mac")
  }
  custom_launcher_class <- R6::R6Class(
    classname = "custom_launcher_class",
    inherit = crew::crew_class_launcher,
    public = list(
      launch_worker = function(call, name, launcher, worker, instance) {
        bin <- if_any(
          tolower(Sys.info()[["sysname"]]) == "windows",
          "R.exe",
          "R"
        )
        path <- file.path(R.home("bin"), bin)
        processx::process$new(command = path, args = c("-e", call))
      },
      terminate_worker = function(handle) {
        handle$kill()
      }
    )
  )
  crew_controller_custom <- function(
    name = "custom controller name",
    workers = 1L,
    host = "127.0.0.1",
    port = NULL,
    tls = crew::crew_tls(mode = "none"),
    seconds_interval = 0.5,
    seconds_timeout = 5,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    launch_max = 5L
  ) {
    client <- crew::crew_client(
      name = name,
      workers = workers,
      host = host,
      port = port,
      tls = tls,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      reset_globals = reset_globals,
      reset_packages = reset_packages,
      reset_options = reset_options,
      garbage_collection = garbage_collection,
      launch_max = launch_max,
      tls = tls
    )
    controller <- crew::crew_controller(
      client = client,
      launcher = launcher
    )
    controller$validate()
    controller
  }
  controller <- crew_controller_custom()
  controller$start()
  on.exit({
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  controller$push(name = "pid", command = ps::ps_pid())
  controller$wait(seconds_timeout = 10, seconds_interval = 0.5)
  out <- controller$pop()$result[[1]]
  handle <- controller$launcher$workers$handle[[1]]
  exp <- handle$get_pid()
  expect_equal(out, exp)
  expect_true(handle$is_alive())
  controller$launcher$terminate()
  crew_retry(
    ~!handle$is_alive(),
    seconds_interval = 0.1,
    seconds_timeout = 5
  )
  expect_false(handle$is_alive())
  controller$launcher$rotate()
  controller$launcher$tally()
  out <- controller$launcher$summary()
  expect_equal(out$launches, 1L)
  controller$terminate()
})

crew_test("custom launcher with local async errors", {
  skip_on_cran()
  skip_on_covr() # Avoid clashes with NNG and covr child processes.
  skip_on_os("windows")
  skip_if_not_installed("processx")
  if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
    skip_on_os("mac")
  }
  custom_launcher_class <- R6::R6Class(
    classname = "custom_launcher_class",
    inherit = crew::crew_class_launcher,
    public = list(
      launch_worker = function(call, name, launcher, worker, instance) {
        self$async$eval(
          command = "okay value",
          packages = "this package does not exist"
        )
      },
      terminate_worker = function(handle) {
        self$async$eval(
          command = stop("termination error"),
          packages = "processx"
        )
      }
    )
  )
  crew_controller_custom <- function(
    name = "custom controller name",
    workers = 1L,
    host = "127.0.0.1",
    port = NULL,
    tls = crew::crew_tls(mode = "none"),
    seconds_interval = 0.5,
    seconds_timeout = 5,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    launch_max = 5L,
    processes = NULL
  ) {
    client <- crew::crew_client(
      name = name,
      workers = workers,
      host = host,
      port = port,
      tls = tls,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      reset_globals = reset_globals,
      reset_packages = reset_packages,
      reset_options = reset_options,
      garbage_collection = garbage_collection,
      launch_max = launch_max,
      tls = tls,
      processes = processes
    )
    controller <- crew::crew_controller(
      client = client,
      launcher = launcher
    )
    controller$validate()
    controller
  }
  controller <- crew_controller_custom(processes = 1L)
  controller$start()
  on.exit({
    try(suppressWarnings(controller$terminate()), silent = TRUE)
    rm(controller)
    gc()
    crew_test_sleep()
  })
  expect_equal(controller$launcher$errors(), NULL)
  controller$launcher$launch(index = 1L)
  controller$launcher$wait()
  expect_crew_error(controller$launcher$launch(index = 1L))
  expect_crew_error(
    controller$launcher$forward(index = 1L, condition = "error")
  )
  expect_warning(
    controller$launcher$forward(index = 1L, condition = "warning"),
    class = "crew_warning"
  )
  expect_message(
    controller$launcher$forward(index = 1L, condition = "message"),
    class = "crew_message"
  )
  out <- controller$launcher$forward(index = 1L, condition = "character")
  expect_equal(out, controller$launcher$errors())
  expect_equal(length(out), 1L)
  expect_true(any(grepl("Worker 1 launch", out)))
  suppressWarnings(
    expect_crew_error(
      expect_warning(controller$terminate(), class = "crew_warning")
    )
  )
  out <- controller$launcher$forward(index = 1L, condition = "character")
  expect_equal(out, controller$launcher$errors())
  expect_equal(length(out), 2L)
  expect_true(any(grepl("Worker 1 launch", out)))
  expect_true(any(grepl("Worker 1 termination", out)))
})

crew_test("custom launcher with async internal launcher tasks", {
  skip_on_cran()
  skip_on_covr() # Avoid clashes with NNG and covr child processes.
  skip_on_os("windows")
  skip_if_not_installed("processx")
  # TODO: remove this part when crew.cluster is updated on CRAN:
  Sys.setenv(TESTTHAT = "false")
  on.exit(Sys.setenv(TESTTHAT = "true"))
  if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
    skip_on_os("mac")
  }
  custom_launcher_class <- R6::R6Class(
    classname = "custom_launcher_class",
    inherit = crew::crew_class_launcher,
    public = list(
      launch_worker = function(call, name, launcher, worker, instance) {
        bin <- if_any(
          tolower(Sys.info()[["sysname"]]) == "windows",
          "R.exe",
          "R"
        )
        path <- file.path(R.home("bin"), bin)
        self$async$eval(
          command = {
            handle <- process$new(command = path, args = c("-e", call))
            while (!handle$is_alive()) {
              Sys.sleep(0.025)
            }
            list(pid = handle$get_pid(), status = "started")
          },
          data = list(
            path = path,
            call = call
          ),
          packages = "processx"
        )
      },
      terminate_worker = function(handle) {
        pid <- handle$data$pid
        self$async$eval(
          command = {
            ps::ps_kill(p = ps::ps_handle(pid = pid))
            list(pid = pid, status = "terminated")
          },
          data = list(pid = pid)
        )
      }
    )
  )
  crew_controller_custom <- function(
    name = "custom controller name",
    workers = 1L,
    host = "127.0.0.1",
    port = NULL,
    tls = crew::crew_tls(mode = "none"),
    seconds_interval = 0.5,
    seconds_timeout = 5,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    launch_max = 5L,
    processes = NULL
  ) {
    client <- crew::crew_client(
      name = name,
      workers = workers,
      host = host,
      port = port,
      tls = tls,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      reset_globals = reset_globals,
      reset_packages = reset_packages,
      reset_options = reset_options,
      garbage_collection = garbage_collection,
      launch_max = launch_max,
      tls = tls,
      processes = processes
    )
    controller <- crew::crew_controller(
      client = client,
      launcher = launcher
    )
    controller$validate()
    controller
  }
  controller <- crew_controller_custom(processes = 1L)
  controller$start()
  on.exit({
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  controller$push(name = "pid", command = ps::ps_pid())
  controller$wait(seconds_timeout = 10, seconds_interval = 0.5)
  out <- controller$pop()$result[[1L]]
  handle <- controller$launcher$workers$handle[[1L]]
  pid <- handle$data$pid
  expect_equal(out, pid)
  expect_equal(handle$data$status, "started")
  controller$launcher$terminate()
  handle <- controller$launcher$workers$termination[[1L]]
  expect_equal(handle$data$pid, pid)
  expect_equal(handle$data$status, "terminated")
})
