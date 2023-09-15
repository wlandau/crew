crew_test("abstract launcher class", {
  out <- crew_launcher(reset_options = -1)
  expect_crew_error(out$validate())
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
    seconds_exit = 4,
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
    seconds_exit = 4,
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
    seconds_exit = 4,
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
      "socket",
      "start",
      "launches",
      "futile",
      "launched",
      "history",
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
  out <- launcher$done(daemons = daemons)
  exp <- c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 12L)
  expect_equal(out, exp)
  launcher$workers$launched <- rep(FALSE, nrow(grid))
  expect_equal(launcher$done(daemons = daemons), integer(0L))
})

crew_test("launcher tally()", {
  grid <- expand.grid(complete = c(3L, 7L), launched = c(TRUE, FALSE))
  launcher <- crew_class_launcher$new(seconds_launch = 9999)
  launcher$start(sockets = rep("x", nrow(grid)))
  launcher$workers$launched <- grid$launched
  daemons <- cbind(assigned = rep(7L, nrow(grid)), complete = grid$complete)
  expect_equal(launcher$workers$assigned, rep(0L, nrow(grid)))
  expect_equal(launcher$workers$complete, rep(0L, nrow(grid)))
  launcher$tally(daemons = daemons)
  expect_equal(launcher$workers$assigned, c(0L, 0L, 7L, 7L))
  expect_equal(launcher$workers$complete, c(0L, 0L, 3L, 7L))
  launcher$workers$launched[1L] <- FALSE
  launcher$workers$launched[4L] <- TRUE
  launcher$tally(daemons = daemons)
  expect_equal(launcher$workers$assigned, c(7L, 0L, 7L, 7L))
  expect_equal(launcher$workers$complete, c(3L, 0L, 3L, 7L))
  launcher$workers$launched <- !(launcher$workers$launched)
  launcher$tally(daemons = daemons)
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

crew_test("launcher backlogged() and resolved()", {
  grid <- expand.grid(
    launched = c(TRUE, FALSE),
    assigned = c(7L, 7L),
    complete = c(3L, 7L)
  )
  launcher <- crew_class_launcher$new(seconds_launch = 9999)
  launcher$start(sockets = rep("x", nrow(grid)))
  for (field in colnames(grid)) {
    launcher$workers[[field]] <- grid[[field]]
  }
  expect_equal(launcher$backlogged(), c(2L, 4L))
  expect_equal(launcher$resolved(), c(6L, 8L))
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
    sort(c("worker", "launches", "assigned", "complete"))
  )
  expect_equal(out$worker, c(1L, 2L))
  for (field in c("launches", "assigned", "complete")) {
    expect_equal(out[[field]], c(0L, 0L))
  }
})

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
    seconds_exit = 1,
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
      seconds_launch = seconds_launch,
      seconds_idle = seconds_idle,
      seconds_wall = seconds_wall,
      seconds_exit = seconds_exit,
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
  walk(x = controller$launcher$done(), f = controller$launcher$rotate)
  controller$launcher$tally()
  out <- controller$launcher$summary()
  for (field in colnames(out)) {
    expect_equal(out[[field]], 1L)
  }
  controller$terminate()
})
