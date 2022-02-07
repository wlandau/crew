test_that("empty crew", {
  expect_silent(class_crew$new()$validate())
})

test_that("nonempty crew", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew)
  crew$workers[[worker$name]] <- worker
  expect_silent(crew$validate())
})

test_that("invalid crew", {
  crew <- class_crew$new()
  worker <- class_worker_callr$new(crew = crew)
  crew$workers[["nope"]] <- worker
  expect_error(crew$validate(), class = "crew_error")
})

test_that("crew recruit", {
  crew <- class_crew$new(
    store = crew::class_store_local$new(),
    worker_classes = list(
      crew::class_worker_callr,
      crew::class_worker_future
    )
  )
  crew$validate()
  crew$recruit(workers = 2)
  crew$validate()
  expect_equal(length(crew$workers), 2L)
  for (worker in crew$workers) {
    expect_true("process" %in% names(worker))
  }
  crew$recruit(workers = 3, class = "worker_future")
  crew$validate()
  expect_equal(length(crew$workers), 5L)
  for (index in seq(3, 5)) {
    expect_true("future" %in% names(crew$workers[[index]]))
  }
  walk(crew$workers, ~.x$validate())
})

test_that("crew launch with tag", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(tags = "x")
  crew$recruit(tags = "y")
  crew$launch(tags = "y")
  expect_false(crew$workers[[1]]$up())
  expect_true(crew$workers[[2]]$up())
})

test_that("crew sendable", {
  crew <- class_crew$new()
  expect_false(crew$sendable())
  crew$recruit(workers = 2, timeout = Inf)
  expect_true(crew$sendable())
  for (worker in crew$workers) {
    worker$assigned <- TRUE
  }
  expect_false(crew$sendable())
})

test_that("crew sendable with tags", {
  crew <- class_crew$new()
  expect_false(crew$sendable())
  crew$recruit(workers = 1, timeout = Inf, tags = "a")
  crew$recruit(workers = 1, timeout = Inf, tags = "b")
  crew$workers[[2]]$assigned <- TRUE
  expect_true(crew$sendable())
  expect_true(crew$sendable(tags = "a"))
  expect_false(crew$sendable(tags = "b"))
  expect_false(crew$sendable(tags = "c"))
})

test_that("crew receivable", {
  crew <- class_crew$new()
  expect_false(crew$receivable())
  crew$recruit(workers = 1, timeout = Inf, tags = "a")
  crew$recruit(workers = 1, timeout = Inf, tags = "b")
  crew$store$write_output(name = crew$workers[[2]]$name, data = "x")
  expect_true(crew$receivable())
  expect_false(crew$receivable(tags = "a"))
  expect_true(crew$receivable(tags = "b"))
  expect_false(crew$receivable(tags = "c"))
})

test_that("crew send and receive", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  crew$send(fun = function(x) x, args = list(x = "y"))
  while (!crew$receivable()) {
    Sys.sleep(0.1)
  }
  expect_equal(crew$receive()$value, "y")
  expect_true(crew$workers[[1]]$up())
  expect_false(crew$workers[[2]]$up())
})

test_that("crew send and receive with a busy worker", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  crew$workers[[1]]$assigned <- TRUE
  crew$send(fun = function(x) x, args = list(x = "y"))
  while (!crew$receivable()) {
    Sys.sleep(0.1)
  }
  expect_equal(crew$receive()$value, "y")
  expect_false(crew$workers[[1]]$up())
  expect_true(crew$workers[[2]]$up())
})

test_that("crew tries to send to already up workers first", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  crew$workers[[2]]$launch()
  while (!crew$workers[[2]]$up()) {
    Sys.sleep(0.1)
  }
  crew$send(fun = function(x) x, args = list(x = "y"))
  while (!crew$receivable()) {
    Sys.sleep(0.1)
  }
  expect_equal(crew$receive()$value, "y")
  expect_false(crew$workers[[1]]$up())
  expect_true(crew$workers[[2]]$up())
})

test_that("crew send and receive at a tag", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  crew$workers[[2]]$tags <- "my_tag"
  crew$send(fun = function(x) x, args = list(x = "y"), tags = "my_tag")
  while (!crew$receivable(tags = "my_tag")) {
    Sys.sleep(0.1)
  }
  expect_equal(crew$receive(tags = "my_tag")$value, "y")
  expect_false(crew$workers[[1]]$up())
  expect_true(crew$workers[[2]]$up())
})

test_that("crew send at a bad tag", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  expect_error(
    crew$send(fun = function(x) x, args = list(x = "y"), tag = "bad"),
    class = "crew_error"
  )
})

test_that("crew receive at a bad tag", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 2, timeout = Inf)
  crew$send(fun = function(x) x, args = list(x = "y"))
  while (!crew$receivable()) {
    Sys.sleep(0.1)
  }
  expect_error(crew$receive(tag = "bad"), class = "crew_error")
})

test_that("crew shutdown sendable_only = TRUE", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 3, timeout = Inf)
  crew$workers[[2]]$assigned <- TRUE
  for (index in seq(2, 3)) {
    crew$workers[[index]]$launch()
  }
  crew$shutdown(workers = 1, sendable_only = TRUE)
  while (crew$workers[[3]]$up()) {
    Sys.sleep(0.1)
  }
  expect_false(crew$workers[[1]]$up())
  expect_true(crew$workers[[2]]$up())
  expect_false(crew$workers[[3]]$up())
})

test_that("crew shutdown sendable_only = FALSE", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 3, timeout = Inf)
  crew$workers[[2]]$assigned <- TRUE
  for (index in seq(2, 3)) {
    crew$workers[[index]]$launch()
  }
  crew$shutdown(workers = 1, sendable_only = FALSE)
  while (crew$workers[[2]]$up()) {
    Sys.sleep(0.1)
  }
  expect_false(crew$workers[[1]]$up())
  expect_false(crew$workers[[2]]$up())
  expect_true(crew$workers[[3]]$up())
})

test_that("crew shutdown one worker", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 3, timeout = Inf)
  for (index in seq(2, 3)) {
    crew$workers[[index]]$launch()
  }
  crew$shutdown(workers = 1)
  while (crew$workers[[2]]$up()) {
    Sys.sleep(0.1)
  }
  expect_false(crew$workers[[1]]$up())
  expect_false(crew$workers[[2]]$up())
  expect_true(crew$workers[[3]]$up())
})

test_that("crew shutdown all workers", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 3, timeout = Inf)
  for (index in seq(2, 3)) {
    crew$workers[[index]]$launch()
  }
  crew$shutdown(workers = 2)
  while (crew$workers[[2]]$up() || crew$workers[[3]]$up()) {
    Sys.sleep(0.1)
  }
  for (index in seq_len(3)) {
    expect_false(crew$workers[[index]]$up())
  }
})

test_that("crew shutdown at tag", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 1, timeout = Inf, tags = "a")
  crew$recruit(workers = 1, timeout = Inf, tags = "b")
  crew$launch()
  for (worker in crew$workers) {
    while (!worker$up()) {
      Sys.sleep(0.1)
    }
  }
  crew$shutdown(tags = "b")
  expect_true(crew$workers[[1]]$up())
  expect_false(crew$workers[[2]]$up())
})

test_that("crew dismiss sendable_only = TRUE, down_only = TRUE", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 4, timeout = Inf)
  names <- names(crew$workers)
  for (index in c(1, 3)) {
    crew$workers[[index]]$launch()
  }
  for (index in c(1, 2)) {
    crew$workers[[index]]$assigned <- TRUE
  }
  crew$dismiss(workers = 2, sendable_only = TRUE, down_only = TRUE)
  expect_equal(sort(names(crew$workers)), sort(names[seq_len(3)]))
  for (worker in crew$workers) {
    expect_true(!worker$sendable() || worker$up())
  }
})

test_that("crew dismiss sendable_only = FALSE, down_only = TRUE", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 4, timeout = Inf)
  names <- names(crew$workers)
  for (index in c(1, 3)) {
    crew$workers[[index]]$launch()
  }
  for (index in c(1, 2)) {
    crew$workers[[index]]$assigned <- TRUE
  }
  crew$dismiss(workers = 2, sendable_only = FALSE, down_only = TRUE)
  expect_equal(sort(names(crew$workers)), sort(names[c(1, 3)]))
  for (worker in crew$workers) {
    expect_true(worker$up())
  }
})

test_that("crew dismiss sendable_only = TRUE, down_only = FALSE", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  crew$recruit(workers = 4, timeout = Inf)
  names <- names(crew$workers)
  workers <- lapply(crew$workers, identity)
  on.exit(walk(workers, ~.x$shutdown()))
  for (index in c(1, 3)) {
    crew$workers[[index]]$launch()
  }
  for (index in c(1, 2)) {
    crew$workers[[index]]$assigned <- TRUE
  }
  crew$dismiss(workers = 2, sendable_only = TRUE, down_only = FALSE)
  expect_equal(sort(names(crew$workers)), sort(names[c(1, 2)]))
  for (worker in crew$workers) {
    expect_true(!worker$sendable())
  }
})

test_that("crew dismiss sendable_only = FALSE, down_only = FALSE", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  crew$recruit(workers = 4, timeout = Inf)
  names <- names(crew$workers)
  workers <- lapply(crew$workers, identity)
  on.exit(walk(workers, ~.x$shutdown()))
  for (index in c(1, 3)) {
    crew$workers[[index]]$launch()
  }
  for (index in c(1, 2)) {
    crew$workers[[index]]$assigned <- TRUE
  }
  crew$dismiss(workers = 4, sendable_only = FALSE, down_only = FALSE)
  expect_equal(length(crew$workers), 0)
})

test_that("crew dismiss some workers", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  crew$recruit(4, timeout = Inf)
  crew$dismiss(workers = 3)
  expect_equal(length(crew$workers), 1)
})

test_that("crew dismiss at tag", {
  crew <- class_crew$new(worker_classes = list(crew::class_worker_callr))
  on.exit(crew$shutdown())
  crew$recruit(workers = 1, timeout = Inf, tags = "a")
  crew$recruit(workers = 1, timeout = Inf, tags = "b")
  crew$dismiss(tags = "b")
  expect_equal(length(crew$workers), 1)
  expect_equal(crew$workers[[1]]$tags, "a")
})

test_that("crew clear everyone", {
  crew <- class_crew$new()
  crew$recruit(workers = 2)
  for (index in seq_len(2)) {
    crew$store$write_input(crew$workers[[index]]$name, "x")
    crew$store$write_output(crew$workers[[index]]$name, "x")
    expect_true(crew$store$exists_input(crew$workers[[index]]$name))
    expect_true(crew$store$exists_output(crew$workers[[index]]$name))
  }
  crew$clear()
  for (index in seq_len(2)) {
    expect_false(crew$store$exists_input(crew$workers[[index]]$name))
    expect_false(crew$store$exists_output(crew$workers[[index]]$name))
  }
})

test_that("crew clear with down_only = TRUE", {
  crew <- class_crew$new()
  crew$recruit(workers = 1)
  on.exit(crew$shutdown())
  crew$launch()
  while (!crew$workers[[1]]$up()) Sys.sleep(0.1)
  crew$send(fun = function() Sys.sleep(200))
  crew$clear(down_only = TRUE)
  expect_true(crew$store$exists_input(crew$workers[[1]]$name))
})

test_that("crew clear with down_only = FALSE", {
  crew <- class_crew$new()
  crew$recruit(workers = 1)
  on.exit(crew$shutdown())
  crew$launch()
  crew$send(fun = function() Sys.sleep(200))
  crew$clear(down_only = FALSE)
  expect_false(crew$store$exists_input(crew$workers[[1]]$name))
  expect_false(crew$store$exists_output(crew$workers[[1]]$name))
})

test_that("crew clear tags ", {
  crew <- class_crew$new()
  crew$recruit(workers = 1, tags = "a")
  crew$recruit(workers = 1, tags = "b")
  for (index in seq_len(2)) {
    crew$store$write_input(crew$workers[[index]]$name, "x")
    crew$store$write_output(crew$workers[[index]]$name, "x")
    expect_true(crew$store$exists_input(crew$workers[[index]]$name))
    expect_true(crew$store$exists_output(crew$workers[[index]]$name))
  }
  crew$clear(tags = "a")
  expect_false(crew$store$exists_input(crew$workers[[1]]$name))
  expect_false(crew$store$exists_output(crew$workers[[1]]$name))
  expect_true(crew$store$exists_input(crew$workers[[2]]$name))
  expect_true(crew$store$exists_output(crew$workers[[2]]$name))
})

test_that("restart stuck worker", {
  crew <- class_crew$new()
  on.exit(crew$shutdown())
  crew$recruit(workers = 1, timeout = Inf)
  worker <- crew$workers[[1]]
  expect_false(worker$stuck())
  crew$send(fun = function() Sys.sleep(Inf))
  expect_false(worker$stuck())
  worker$process$kill()
  while (worker$up()) Sys.sleep(0.1)
  expect_false(worker$up())
  expect_true(worker$stuck())
  worker$restart()
  expect_true(worker$up())
  expect_false(worker$stuck())
})
