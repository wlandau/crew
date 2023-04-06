crew_test("abstract launcher class", {
  expect_silent(crew_launcher()$validate)
  expect_crew_error(crew_launcher(cleanup = -1)$validate())
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
    cleanup = TRUE
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
    cleanup = TRUE
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
  expect_true(settings$cleanup)
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
    cleanup = TRUE,
    seconds_interval = 0.1,
    seconds_timeout = 0.25
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
  skip_on_cran()
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
  skip_on_cran()
  launcher <- crew_class_launcher$new(seconds_launch = 60)
  launcher$start(workers = 3L)
  launcher$workers$start <- c(NA_real_, -Inf, Inf)
  expect_equal(launcher$launching(), c(FALSE, FALSE, TRUE))
})
