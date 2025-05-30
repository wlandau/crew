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
      launch_worker = function(call, name, launcher, worker) {
        bin <- if_any(
          tolower(Sys.info()[["sysname"]]) == "windows",
          "R.exe",
          "R"
        )
        path <- file.path(R.home("bin"), bin)
        processx::process$new(command = path, args = c("-e", call))
      },
      terminate_worker = function(handle) {
        handle$signal(signal = crew::crew_terminate_signal())
      }
    )
  )
  crew_controller_custom <- function(
    name = "custom controller name",
    workers = 1L,
    host = "127.0.0.1",
    port = NULL,
    tls = crew::crew_tls(mode = "none"),
    serialization = NULL,
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
    crashes_error = 5L
  ) {
    client <- crew::crew_client(
      host = host,
      port = port,
      tls = tls,
      serialization = serialization,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      workers = workers,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      crashes_error = crashes_error,
      tls = tls
    )
    controller <- crew::crew_controller(
      client = client,
      launcher = launcher,
      reset_globals = reset_globals,
      reset_packages = reset_packages,
      reset_options = reset_options,
      garbage_collection = garbage_collection,
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
  controller$wait(seconds_timeout = 10)
  out <- controller$pop()$result[[1]]
  handle <- controller$launcher$instances$handle[[1]]
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
  controller$terminate()
})

crew_test("custom launcher with local asyncs launch errors", {
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
      launch_worker = function(call, name, launcher, worker) {
        self$async$eval(
          command = "okay value",
          packages = "this package does not exist"
        )
      },
      terminate_worker = function(handle) {
      }
    )
  )
  crew_controller_custom <- function(
    name = "custom controller name",
    workers = 1L,
    host = "127.0.0.1",
    port = NULL,
    tls = crew::crew_tls(mode = "none"),
    serialization = NULL,
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
    crashes_error = 5L,
    processes = NULL
  ) {
    client <- crew::crew_client(
      host = host,
      port = port,
      tls = tls,
      serialization = serialization,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      workers = workers,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      crashes_error = crashes_error,
      tls = tls,
      processes = processes
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
  controller <- crew_controller_custom(processes = 1L)
  controller$start()
  on.exit({
    try(suppressWarnings(controller$terminate()), silent = TRUE)
    rm(controller)
    gc()
    crew_test_sleep()
  })
  envir <- new.env(parent = emptyenv())
  crew_retry(
    ~ tryCatch(
      expr = {
        controller$push(command = TRUE)
        envir$result <- FALSE
        FALSE
      },
      crew_error = function(condition) {
        envir$result <- TRUE
        TRUE
      }
    ),
    seconds_interval = 1,
    seconds_timeout = 30
  )
  expect_true(envir$result)
})

crew_test("custom launcher with local asyncs termination errors", {
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
      launch_worker = function(call, name, launcher, worker) {
        list(abstract = TRUE)
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
    serialization = NULL,
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
    crashes_error = 5L,
    processes = NULL
  ) {
    client <- crew::crew_client(
      host = host,
      port = port,
      tls = tls,
      serialization = serialization,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      workers = workers,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      crashes_error = crashes_error,
      tls = tls,
      processes = processes
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
  controller <- crew_controller_custom(processes = 1L)
  controller$start()
  on.exit({
    try(suppressWarnings(controller$terminate()), silent = TRUE)
    rm(controller)
    gc()
    crew_test_sleep()
  })
  controller$launch(n = 1L)
  expect_warning(controller$terminate(), class = "crew_warning")
})

crew_test("custom launcher with async internal launcher tasks", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr() # Avoid clashes with NNG and covr child processes.
  skip_on_os("windows")
  skip_if_not_installed("processx")
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
            handle <- process$new(
              command = path,
              args = c("-e", call),
              cleanup = FALSE
            )
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
        pid <- handle$pid
        self$async$eval(
          command = {
            crew::crew_terminate_process(pid)
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
    serialization = NULL,
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
    crashes_error = 5L,
    processes = NULL
  ) {
    client <- crew::crew_client(
      host = host,
      port = port,
      tls = tls,
      serialization = serialization,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      workers = workers,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout,
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      tasks_max = tasks_max,
      tasks_timers = tasks_timers,
      crashes_error = crashes_error,
      tls = tls,
      processes = processes
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
  controller <- crew_controller_custom(processes = 1L)
  controller$start()
  on.exit({
    controller$terminate()
    rm(controller)
    gc()
    crew_test_sleep()
  })
  controller$push(name = "pid", command = ps::ps_pid())
  controller$wait(seconds_timeout = 10)
  envir <- new.env(parent = emptyenv())
  crew_retry(
    ~ {
      envir$pid <- controller$pop()$result[[1L]]
      !is.null(envir$pid)
    },
    seconds_interval = 0.25,
    seconds_timeout = 15
  )
  handle <- controller$launcher$instances$handle[[1L]]
  pid <- handle$pid
  expect_equal(envir$pid, pid)
  expect_equal(handle$status, "started")
  controller$launcher$terminate()
})

crew_test("can terminate a lost worker with an async launch", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_covr() # Avoid clashes with NNG and covr child processes.
  if (isTRUE(as.logical(Sys.getenv("CI", "false")))) {
    skip_on_os("mac")
  }
  custom_launcher_class <- R6::R6Class(
    classname = "custom_launcher_class",
    inherit = crew::crew_class_launcher,
    public = list(
      launch_worker = function(call, name, launcher, worker) {
      },
      terminate_worker = function(handle) {
        ps::ps_kill(p = ps::ps_handle(handle$pid))
      }
    )
  )
  crew_controller_custom <- function(
    name = "custom controller name",
    workers = 1L,
    host = "127.0.0.1",
    port = NULL,
    tls = crew::crew_tls(mode = "none"),
    serialization = NULL,
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
    crashes_error = 5L
  ) {
    client <- crew::crew_client(
      host = host,
      port = port,
      tls = tls,
      serialization = serialization,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
      workers = workers,
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
      crashes_error = crashes_error,
      tls = tls
    )
    controller <- crew::crew_controller(
      client = client,
      launcher = launcher
    )
    controller$validate()
    controller
  }
  x <- crew_controller_custom()
  x$start()
  on.exit({
    x$terminate()
    rm(x)
    gc()
    crew_test_sleep()
  })
  private <- crew_private(x$launcher)
  bin <- ifelse(
    tolower(Sys.info()[["sysname"]]) == "windows",
    "R.exe",
    "R"
  )
  path <- file.path(R.home("bin"), bin)
  handle <- mirai::mirai(
    .expr = {
      Sys.sleep(0.5)
      handle <- processx::process$new(
        command = path,
        args = c("-e", "Sys.sleep(90)"),
        cleanup = FALSE
      )
      list(pid = handle$get_pid())
    },
    .args = list(path = path)
  )
  private$.instances <- tibble::add_row(
    private$.instances,
    handle = list(handle),
    id = 99L,
    start = - Inf,
    online = FALSE,
    discovered = FALSE
  )
  crew_retry(
    ~ {
      x$scale()
      !nanonext::.unresolved(handle)
    },
    seconds_interval = 0.1,
    seconds_timeout = 15
  )
  expect_true(is.integer(handle$data$pid))
  crew_retry(
    ~ {
      x$scale()
      tryCatch(
        !ps::ps_is_running(p = ps::ps_handle(handle$data$pid)),
        error = function(condition) {
          TRUE
        }
      )
    },
    seconds_interval = 0.1,
    seconds_timeout = 15
  )
  expect_equal(nrow(x$launcher$instances), 0L)
  x$client$terminate()
})
