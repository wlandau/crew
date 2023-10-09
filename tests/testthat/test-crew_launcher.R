crew_test("abstract launcher class", {
  out <- crew_launcher(reset_options = -1)
  expect_crew_error(out$validate())
})

crew_test("default launch_launcher() method", {
  launcher <- crew_class_launcher$new()
  out <- launcher$launch_worker(
    call = "a",
    name = "b",
    launcher = "c",
    worker = 57L,
    instance = "d"
  )
  expect_equal(out$abstract, TRUE)
})

crew_test("default terminate_launcher() method", {
  launcher <- crew_class_launcher$new()
  expect_equal(launcher$terminate_worker(handle = crew_null)$abstract, TRUE)
})

crew_test("launcher settings", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = TRUE,
    reset_packages = TRUE,
    reset_options = TRUE,
    garbage_collection = TRUE,
    tls = crew_tls()
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
  expect_equal(settings$cleanup, 15L)
})

crew_test("launcher alternative cleanup", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = FALSE,
    reset_packages = TRUE,
    reset_options = FALSE,
    garbage_collection = TRUE,
    tls = crew_tls()
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
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = TRUE,
    garbage_collection = FALSE,
    tls = crew_tls()
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
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = FALSE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    tls = crew_tls()
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
    tasks_max = 7,
    tasks_timers = 8,
    reset_globals = TRUE,
    reset_packages = FALSE,
    reset_options = FALSE,
    garbage_collection = FALSE,
    tls = crew_tls()
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
  skip_on_os("windows")
  launcher <- crew_class_launcher$new()
  workers <- launcher$workers
  expect_equal(workers, NULL)
  launcher$start(sockets = c("a", "b"))
  workers <- launcher$workers
  expect_equal(nrow(workers), 2L)
  expect_equal(
    colnames(workers),
    cols <- c(
      "handle",
      "termination",
      "socket",
      "start",
      "launches",
      "futile",
      "launched",
      "terminated",
      "history",
      "online",
      "discovered",
      "assigned",
      "complete"
    )
  )
  expect_equal(workers$handle, list(crew_null, crew_null))
  expect_equal(workers$socket, c("a", "b"))
  expect_equal(workers$start, c(NA_real_, NA_real_))
  expect_equal(workers$launches, rep(0L, 2L))
  expect_equal(workers$launched, rep(FALSE, 2L))
  expect_equal(workers$assigned, rep(0L, 2L))
  expect_equal(workers$complete, rep(0L, 2L))
})

crew_test("launcher done()", {
  grid <- expand.grid(
    complete = c(3L, 7L),
    start = c(NA_real_, -Inf, Inf),
    instance = c(0L, 1L),
    online = c(0L, 1L)
  )
  launcher <- crew_class_launcher$new(seconds_launch = 9999)
  launcher$start(sockets = rep("x", nrow(grid)))
  launcher$workers$start <- grid$start
  socket <- sprintf(
    "ws://127.0.0.1:5000/%s/token",
    seq_len(nrow(grid))
  )
  launcher$workers$socket <- socket
  launcher$workers$launched <- rep(TRUE, nrow(grid))
  daemons <- cbind(
    online = grid$online,
    instance = grid$instance,
    assigned = rep(7L, nrow(grid)),
    complete = grid$complete
  )
  expect_equal(launcher$workers$assigned, rep(0L, nrow(grid)))
  expect_equal(launcher$workers$complete, rep(0L, nrow(grid)))
  launcher$tally(daemons = daemons)
  out <- which(launcher$done())
  exp <- c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 12L)
  expect_equal(out, exp)
  launcher$workers$launched <- rep(FALSE, nrow(grid))
  launcher$tally(daemons = daemons)
  expect_equal(which(launcher$done()), integer(0L))
})

crew_test("launcher tally()", {
  grid <- expand.grid(complete = c(3L, 7L), launched = c(TRUE, FALSE))
  launcher <- crew_class_launcher$new(seconds_launch = 9999)
  launcher$start(sockets = rep("x", nrow(grid)))
  launcher$workers$launched <- grid$launched
  daemons <- cbind(
    online = c(1L, 1L, 0L, 0L),
    instance = c(1L, 0L, 1L, 0L),
    assigned = rep(7L, nrow(grid)),
    complete = grid$complete
  )
  expect_equal(launcher$workers$online, rep(FALSE, nrow(grid)))
  expect_equal(launcher$workers$discovered, rep(FALSE, nrow(grid)))
  expect_equal(launcher$workers$assigned, rep(0L, nrow(grid)))
  expect_equal(launcher$workers$complete, rep(0L, nrow(grid)))
  launcher$tally(daemons = daemons)
  expect_equal(launcher$workers$online, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(launcher$workers$discovered, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(launcher$workers$assigned, c(7L, 7L, 7L, 7L))
  expect_equal(launcher$workers$complete, c(3L, 7L, 3L, 7L))
})

crew_test("launcher unlaunched()", {
  launcher <- crew_class_launcher$new(seconds_launch = 9999)
  launcher$start(sockets = rep("x", 5L))
  launcher$workers$launched <- c(TRUE, FALSE, FALSE, FALSE, TRUE)
  expect_equal(launcher$unlaunched(), c(2L, 3L, 4L))
  expect_equal(launcher$unlaunched(n = 2L), c(2L, 3L))
})

crew_test("launcher summary", {
  x <- crew_launcher()
  expect_null(x$summary())
  x$start(sockets = c("a", "b"))
  out <- x$summary()
  expect_true(tibble::is_tibble(out))
  expect_equal(nrow(out), 2L)
  expect_equal(
    sort(colnames(out)),
    sort(
      c("worker", "launches", "online", "discovered", "assigned", "complete")
    )
  )
  expect_equal(out$worker, c(1L, 2L))
  for (field in c("launches", "assigned", "complete")) {
    expect_equal(out[[field]], c(0L, 0L))
  }
  for (field in c("online", "discovered")) {
    expect_equal(out[[field]], c(FALSE, FALSE))
  }
})

crew_test("deprecate seconds_exit", {
  suppressWarnings(crew_launcher(seconds_exit = 1))
  expect_true(TRUE)
})

crew_test("deprecate throttle()", {
  skip_on_cran()
  skip_on_os("windows")
  x <- crew_launcher()
  suppressWarnings(x$throttle())
  expect_true(TRUE)
})
