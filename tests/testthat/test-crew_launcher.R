crew_test("abstract launcher class", {
  expect_silent(crew_launcher()$validate)
  expect_crew_error(crew_launcher(reset_options = -1)$validate())
})

crew_test("default terminate_launcher() method", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE
  )
  expect_null(launcher$terminate_worker())
})

crew_test("launcher settings", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = TRUE,
    reset_packages = TRUE,
    reset_options = TRUE,
    garbage_collection = TRUE
  )
  expect_equal(launcher$name, "my_launcher_name")
  socket <- "ws://127.0.0.1:5000"
  settings <- launcher$settings(socket = socket)
  expect_equal(settings$url, socket)
  expect_equal(settings$asyncdial, FALSE)
  expect_equal(settings$maxtasks, 7)
  expect_equal(settings$idletime, 2000)
  expect_equal(settings$walltime, 3000)
  expect_equal(settings$timerstart, 8)
  expect_equal(settings$exitlinger, 4000)
  expect_equal(settings$cleanup, 15L)
})

crew_test("launcher alternative cleanup", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = FALSE,
    reset_packages = TRUE,
    reset_options = FALSE,
    garbage_collection = TRUE
  )
  settings <- launcher$settings(socket = "ws://127.0.0.1:5000")
  expect_equal(settings$cleanup, 10L)
})

crew_test("launcher alternative cleanup 2", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = TRUE,
    garbage_collection = FALSE
  )
  settings <- launcher$settings(socket = "ws://127.0.0.1:5000")
  expect_equal(settings$cleanup, 5L)
})

crew_test("launcher alternative cleanup 3", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = FALSE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE
  )
  settings <- launcher$settings(socket = "ws://127.0.0.1:5000")
  expect_equal(settings$cleanup, 0L)
})

crew_test("launcher call", {
  skip_on_cran()
  skip_on_os("windows")
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 0.001,
    seconds_wall = 3,
    seconds_exit = 4,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE
  )
  out <- launcher$call(
    socket = "ws://127.0.0.1:5000/3/cba033e58",
    launcher = "launcher_a",
    worker = 3L,
    instance = "cba033e58"
  )
  expect_true(is.character(out))
  expect_true(!anyNA(out))
  expect_equal(length(out), 1L)
  expect_true(all(nzchar(out)))
  expect_true(grepl(pattern = "^crew::crew_worker\\(", x = out))
  message <- tryCatch(eval(parse(text = out)), error = conditionMessage)
  expect_match(message, regexp = "denied|refused")
})

crew_test("launcher start()", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  launcher <- crew_class_launcher$new()
  workers <- launcher$workers
  expect_equal(workers, NULL)
  launcher$start(workers = 2L)
  workers <- launcher$workers
  expect_equal(dim(workers), c(2L, 4L))
  expect_equal(
    colnames(workers),
    c("handle", "socket", "start", "launches")
  )
  expect_equal(workers$handle, list(crew_null, crew_null))
  expect_equal(workers$socket, c(NA_character_, NA_character_))
  expect_equal(workers$start, c(NA_real_, NA_real_))
  expect_equal(workers$launches, rep(0L, 2L))
})

crew_test("launcher launching()", {
  skip_if_low_dep_versions()
  skip_on_cran()
  launcher <- crew_class_launcher$new(seconds_launch = 60)
  launcher$start(workers = 3L)
  launcher$workers$start <- c(NA_real_, -Inf, Inf)
  expect_equal(launcher$launching(), c(FALSE, FALSE, TRUE))
})

crew_test("custom launcher", {
  skip_if_low_dep_versions()
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("processx")
  custom_launcher_class <- R6::R6Class(
    classname = "custom_launcher_class",
    inherit = crew::crew_class_launcher,
    public = list(
      launch_worker = function(call, launcher, worker, instance) {
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
    host = NULL,
    port = NULL,
    seconds_interval = 0.5,
    seconds_timeout = 5,
    seconds_launch = 30,
    seconds_idle = Inf,
    seconds_wall = Inf,
    seconds_exit = 1,
    tasks_max = Inf,
    tasks_timers = 0L,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE
  ) {
    router <- crew::crew_router(
      name = name,
      workers = workers,
      host = host,
      port = port,
      seconds_interval = seconds_interval,
      seconds_timeout = seconds_timeout
    )
    launcher <- custom_launcher_class$new(
      name = name,
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
    controller <- crew::crew_controller(
      router = router,
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
})
