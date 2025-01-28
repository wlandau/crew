test_that("backlog of tasks completes with finite seconds_idle", {
  old_options <- list(
    cli.ansi = getOption("cli.ansi"),
    cli.unicode = getOption("cli.unicode"),
    cli.dynamic = getOption("cli.dynamic")
  )
  options(
    cli.ansi = TRUE,
    cli.unicode = TRUE,
    cli.dynamic = TRUE
  )
  on.exit(options(old_options))
  controller <- crew_controller_local(
    workers = 20L,
    seconds_idle = 1
  )
  on.exit(controller$terminate(), add = TRUE)
  controller$start()
  names <- character(0L)
  index <- 0L
  n_tasks <- 60000L
  cli::cli_progress_bar(
    name = "seconds_idle",
    type = "custom",
    format = paste(
      "{cli::pb_current}/{cli::pb_total}",
      "{cli::pb_bar}",
      "{cli::pb_percent}",
      "ETA {cli::pb_eta}"
    ),
    total = n_tasks
  )
  on.exit(cli::cli_progress_done(), add = TRUE)
  time <- system.time(
    while (index < n_tasks || !(controller$empty())) {
      if (index < n_tasks) {
        index <- index + 1L
        controller$push(
          name = as.character(index),
          command = Sys.sleep(0.005)
        )
      }
      out <- controller$pop()
      if (!is.null(out)) {
        names[[length(names) + 1L]] <- out$name
        cli::cli_progress_update()
      }
    }
  )
  cli::cli_progress_done()
  cli::cli_alert_success(paste(time["elapsed"], "seconds"))
  testthat::expect_equal(sort(as.integer(names)), seq_len(n_tasks))
  testthat::expect_equal(controller$unresolved(), 0L)
  testthat::expect_equal(length(controller$tasks), 0L)
  controller$terminate()
})
