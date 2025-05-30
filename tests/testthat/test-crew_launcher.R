crew_test("abstract launcher class", {
  skip_on_cran()
  expect_silent(crew_launcher())
  expect_crew_error(crew_launcher(seconds_idle = -1))
})

crew_test("validate a started launcher", {
  skip_on_cran()
  skip_on_os("windows")
  out <- crew_launcher(processes = 1L)
  out$start(url = "url", profile = "profile")
  on.exit(out$terminate())
  expect_s3_class(out$async, "crew_class_async")
  expect_s3_class(out$throttle, "crew_class_throttle")
  expect_silent(out$async$validate())
  expect_silent(out$throttle$validate())
  expect_silent(out$validate())
})

crew_test("active bindings for covr", {
  skip_on_cran()
  skip_on_os("windows")
  out <- crew_launcher(processes = 1L, r_arguments = "--vanilla")
  expect_true(is.character(out$name))
  expect_equal(out$workers, 1L)
  expect_true(is.numeric(out$seconds_interval))
  expect_true(is.numeric(out$seconds_timeout))
  expect_true(is.numeric(out$seconds_launch))
  expect_true(is.numeric(out$seconds_idle))
  expect_true(is.numeric(out$seconds_wall))
  expect_true(is.numeric(out$tasks_max))
  expect_true(is.numeric(out$tasks_timers))
  expect_true(inherits(out$tls, "crew_class_tls"))
  expect_equal(out$processes, 1L)
  expect_equal(out$r_arguments, "--vanilla")
  expect_s3_class(
    out$options_metrics,
    c("crew_options_metrics", "crew_options")
  )
  expect_null(out$url)
  expect_null(out$profile)
  expect_true(is.data.frame(out$instances))
  expect_null(out$id, 0L)
  expect_null(out$async)
  expect_null(out$throttle)
})

crew_test("preemptive async termination for covr", {
  skip_on_cran()
  skip_on_os("windows")
  out <- crew_launcher(processes = 1L)
  private <- crew_private(out)
  private$.async <- crew_async()
  on.exit({
    private$.async$terminate()
    out$terminate()
  })
  out$start(url = "url", profile = "profile")
  expect_true(TRUE)
})

crew_test("default launch_launcher() method", {
  skip_on_cran()
  launcher <- crew_class_launcher$new(seconds_interval = 0.5)
  out <- launcher$launch_worker(
    call = "a",
    name = "b",
    launcher = "c",
    worker = 57L
  )
  expect_equal(out$abstract, TRUE)
})

crew_test("default terminate_launcher() method", {
  launcher <- crew_class_launcher$new(seconds_interval = 0.5)
  expect_equal(launcher$terminate_worker(handle = crew_null)$abstract, TRUE)
})

crew_test("launcher settings", {
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_interval = 0.5,
    seconds_launch = 1,
    seconds_idle = 2,
    seconds_wall = 3,
    tasks_max = 7,
    tasks_timers = 8,
    tls = crew_tls()
  )
  launcher$start(url = "url", profile = "profile")
  on.exit(launcher$terminate())
  expect_equal(launcher$name, "my_launcher_name")
  settings <- launcher$settings()
  expect_equal(settings$url, "url")
  expect_equal(settings$maxtasks, 7)
  expect_equal(settings$idletime, 2000)
  expect_equal(settings$walltime, 3000)
  expect_equal(settings$timerstart, 8)
  expect_false(settings$cleanup)
  expect_equal(settings$id, 1L)
})

crew_test("launcher call", {
  skip_on_cran()
  skip_on_os("windows")
  launcher <- crew_class_launcher$new(
    name = "my_launcher_name",
    seconds_interval = 0.5,
    seconds_launch = 1,
    seconds_idle = 0.001,
    seconds_wall = 3,
    tasks_max = 7,
    tasks_timers = 8,
    tls = crew_tls()
  )
  launcher$start(url = "url", profile = "profile")
  on.exit(launcher$terminate())
  out <- launcher$call(worker = "cba033e58")
  expect_true(is.character(out))
  expect_true(!anyNA(out))
  expect_equal(length(out), 1L)
  expect_true(all(nzchar(out)))
  expect_true(grepl(pattern = "^crew::crew_worker\\(", x = out))
})

crew_test("launcher update() with no new events", {
  skip_on_cran()
  instances <- expand.grid(
    submitted = FALSE,
    online = c(TRUE, FALSE),
    discovered = c(TRUE, FALSE),
    start = c(Inf, -Inf)
  )
  instances$id <- seq_len(nrow(instances))
  instances$handle <- replicate(nrow(instances), list(), simplify = FALSE)
  instances <- instances[, colnames(launcher_empty_instances)]
  instances <- tibble::as_tibble(instances)
  launcher <- crew_class_launcher$new(
    seconds_launch = 9999,
    seconds_interval = 1
  )
  launcher$start(url = "url", profile = "profile")
  on.exit(launcher$terminate())
  private <- crew_private(launcher)
  private$.instances <- instances
  launcher$update(status = list())
  instances$submitted <- TRUE
  names <- setdiff(colnames(launcher_empty_instances), "handle")
  expect_equal(
    launcher$instances[, names],
    instances[c(1L, 3L, 4L, 5L, 7L), names]
  )
  expect_true(all(launcher$instances$submitted))
})

crew_test("launcher update() with connects", {
  skip_on_cran()
  instances <- expand.grid(
    submitted = FALSE,
    online = c(FALSE, FALSE),
    discovered = c(FALSE, FALSE),
    start = c(Inf, Inf)
  )
  instances$id <- rev(seq_len(nrow(instances)) + 100L)
  instances$handle <- replicate(nrow(instances), list(), simplify = FALSE)
  instances <- instances[, colnames(launcher_empty_instances)]
  instances <- tibble::as_tibble(instances)
  launcher <- crew_class_launcher$new(
    seconds_launch = 9999,
    seconds_interval = 1
  )
  launcher$start(url = "url", profile = "profile")
  on.exit(launcher$terminate())
  private <- crew_private(launcher)
  private$.instances <- instances
  launcher$update(status = list(events = c(102L, 103L, 107L)))
  expect_equal(
    launcher$instances$online,
    instances$id %in% c(102L, 103L, 107L)
  )
  expect_equal(
    launcher$instances$discovered,
    instances$id %in% c(102L, 103L, 107L)
  )
  expect_true(all(launcher$instances$submitted))
})

crew_test("launcher update() with connects and disconnects", {
  skip_on_cran()
  set.seed(0L)
  instances <- expand.grid(
    submitted = FALSE,
    online = c(FALSE, FALSE),
    discovered = c(FALSE, FALSE),
    start = c(Inf, Inf)
  )
  instances$id <- sample(seq_len(nrow(instances)))
  instances$handle <- replicate(nrow(instances), list(), simplify = FALSE)
  instances <- instances[, colnames(launcher_empty_instances)]
  instances <- tibble::as_tibble(instances)
  launcher <- crew_class_launcher$new(
    seconds_launch = 9999,
    seconds_interval = 1
  )
  launcher$start(url = "url", profile = "profile")
  on.exit(launcher$terminate())
  private <- crew_private(launcher)
  private$.instances <- instances
  launcher$update(status = list(events = c(2L, 3L, 7L, -7L)))
  expect_equal(launcher$instances$id, setdiff(instances$id, 7L))
  expect_equal(
    launcher$instances$online,
    launcher$instances$id %in% c(2L, 3L)
  )
  expect_equal(
    launcher$instances$discovered,
    launcher$instances$id %in% c(2L, 3L)
  )
  expect_true(all(launcher$instances$submitted))
})

crew_test("launcher update() with just disconnects", {
  skip_on_cran()
  set.seed(0L)
  instances <- expand.grid(
    submitted = FALSE,
    online = c(TRUE, TRUE),
    discovered = c(TRUE, TRUE),
    start = c(Inf, Inf)
  )
  instances$id <- sample(seq_len(nrow(instances)))
  instances$handle <- replicate(nrow(instances), list(), simplify = FALSE)
  instances <- instances[, colnames(launcher_empty_instances)]
  instances <- tibble::as_tibble(instances)
  launcher <- crew_class_launcher$new(
    seconds_launch = 9999,
    seconds_interval = 1
  )
  launcher$start(url = "url", profile = "profile")
  on.exit(launcher$terminate())
  private <- crew_private(launcher)
  private$.instances <- instances
  launcher$update(status = list(events = c(-4L, -6L)))
  expect_equal(launcher$instances$id, setdiff(instances$id, c(4L, 6L)))
  expect_equal(launcher$instances$online, rep(TRUE, 6L))
  expect_equal(launcher$instances$discovered, rep(TRUE, 6L))
  expect_true(all(launcher$instances$submitted))
})

crew_test("deprecate seconds_exit", {
  suppressWarnings(crew_launcher(seconds_exit = 1))
  expect_true(TRUE)
})

crew_test("deprecated crashes() method", {
  x <- crew_launcher_local()
  expect_equal(x$crashes(index = 1L), 1L)
})

crew_test("deprecated set_name() method", {
  x <- crew_launcher_local(name = "x")
  x$set_name(name = "y")
  expect_equal(x$name, "y")
})
