# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
queue_future <- R6::R6Class(
  classname = "queue_future",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    tasks = NULL,
    results = NULL,
    workers = NULL,
    store = NULL,
    jobs = NULL,
    timeout = NULL,
    wait = NULL,
    initialize_tasks = function() {
      private$tasks <- tibble::tibble(
        task = character(0),
        fun = list(NULL),
        args = list(NULL),
        shutdown = logical(0)
      )
    },
    initialize_results = function() {
      private$results <- tibble::tibble(
        task = character(0),
        result = list(NULL)
      )
    },
    initialize_workers = function(workers) {
      private$workers <- tibble::tibble(
        worker = uuid::UUIDgenerate(n = workers),
        handle = replicate(workers, NULL),
        free = rep(TRUE, workers),
        sent = rep(FALSE, workers),
        up = rep(FALSE, workers),
        done = rep(FALSE, workers),
        lock = rep(FALSE, workers),
        task = rep(NA_character_, workers),
        fun = replicate(workers, NULL),
        args = replicate(workers, NULL),
        shutdown = rep(NA, workers)
      )
    },
    add_task = function(task, fun, args, shutdown = FALSE) {
      dup <- task %in% private$tasks$task || task %in% private$workers$task
      crew_assert(!dup, paste("duplicate task name", task))
      args <- list(
        .data = private$tasks,
        task = task,
        fun = list(fun),
        args = list(args),
        shutdown = shutdown
      )
      if (shutdown) {
        args$.before <- 1L
      }
      private$tasks <- do.call(what = tibble::add_row, args = args)
    },
    add_result = function(task, result) {
      private$results <- tibble::add_row(
        .data = private$results,
        task = task,
        result = list(result)
      )
    },
    send_tasks = function() {
      while (nrow(private$tasks) && any(private$workers$free)) {
        index <- min(which(private$workers$free))
        for (field in colnames(private$tasks)) {
          private$workers[[field]][index] <- private$tasks[[field]][1]
        }
        for (field in c("free", "sent", "done")) {
          private$workers[[field]][index] <- FALSE
        }
        private$tasks <- private$tasks[-1, ]
      }
    },
    send_workers = function() {
      workers <- private$workers
      which <- !workers$free & !workers$sent
      workers <- workers[which, ]
      for (worker in workers$worker) {
        index <- which(private$workers$worker == worker)
        private$send_worker(
          worker = worker,
          fun = private$workers$fun[[index]],
          args = private$workers$args[[index]],
          shutdown = private$workers$shutdown[index]
        )
      }
    },
    send_worker = function(worker, fun, args, shutdown) {
      index <- which(private$workers$worker == worker)
      handle <- private$workers$handle[[index]]
      private$workers$up[index] <- if_any(
        is.null(handle),
        FALSE,
        !future::resolved(handle)
      )
      if (!private$workers$up[index] && !shutdown) {
        private$workers$handle[[index]] <- private$launch_worker(worker)
      }
      task <- list(fun = deparse(fun), args = args)
      private$store$write_worker_input(worker = worker, value = task)
      private$workers$sent[index] <- TRUE
    },
    launch_worker = function(worker) {
      expr <- quote(
        crew::crew_worker(
          worker = worker,
          store = store,
          jobs = jobs,
          timeout = timeout,
          wait = wait
        )
      )
      globals <- list(
        worker = worker,
        store = private$store$marshal(),
        jobs = private$jobs,
        timeout = private$timeout,
        wait = private$wait
      )
      handle <- future::future(
        expr = expr,
        substitute = FALSE,
        packages = "crew",
        globals = globals,
        lazy = FALSE,
        seed = TRUE
      )
      crew_wait(
        fun = function(handle) !future::resolved(handle),
        args = list(handle = handle),
        timeout = private$timeout,
        wait = private$wait
      )
      handle
    },
    update_up = function() {
      private$workers$up <- map_lgl(
        private$workers$handle,
        ~(!is.null(.x) && !future::resolved(.x))
      )
    },
    update_done = function() {
      names <- private$store$list_worker_output()
      private$workers$done[private$workers$worker %in% names] <- TRUE
    },
    update_crashed = function() {
      x <- private$workers
      crashed <- x$sent & !x$done & !x$up
      if (any(crashed)) {
        self$shutdown(wait = FALSE)
        crew_error("worker crashed. Scheduling worker shutdown.")
      }
    },
    update_results = function() {
      for (index in seq_len(nrow(private$workers))) {
        if (private$workers$done[index]) {
          worker <- private$workers$worker[index]
          task <- private$workers$task[index]
          result <- private$store$read_worker_output(worker = worker)
          private$store$delete_worker_output(worker = worker)
          private$add_result(task = task, result = result)
          private$workers$free[index] <- TRUE
          private$workers$sent[index] <- FALSE
          private$workers$done[index] <- FALSE
          private$workers$task[index] <- NA_character_
          private$workers$fun[index] <- list(NULL)
          private$workers$args[index] <- list(NULL)
          private$workers$shutdown[index] <- NA
        }
      }
    },
    update = function(crashed = TRUE) {
      private$update_up()
      private$update_done()
      if (crashed) {
        private$update_crashed()
      }
      private$update_results()
      private$send_tasks()
      private$send_workers()
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      jobs = Inf,
      timeout = 60,
      wait = 0.1
    ) {
      private$store <- store_local$new(timeout = timeout, wait = wait)
      private$jobs <- jobs
      private$timeout <- timeout
      private$wait <- wait
      private$initialize_tasks()
      private$initialize_results()
      private$initialize_workers(workers = workers)
      invisible()
    },
    get_tasks = function() {
      private$tasks
    },
    get_results = function() {
      private$results
    },
    get_workers = function() {
      private$workers
    },
    push = function(
      fun,
      args = list(),
      task = uuid::UUIDgenerate(),
      update = TRUE
    ) {
      fun <- rlang::as_function(fun)
      private$add_task(fun = fun, args = args, task = task)
      if (update) {
        private$update()
      }
      invisible()
    },
    pop = function(update = TRUE) {
      if (update) {
        private$update()
      }
      results <- private$results
      out <- NULL
      if (nrow(results)) {
        out <- list(task = results$task[1], result = results$result[[1]])
        private$results <- private$results[-1, ]
      }
      out
    },
    shutdown = function(wait = TRUE) {
      private$initialize_tasks()
      replicate(
        nrow(private$workers),
        private$add_task(
          fun = crew_shutdown,
          args = list(),
          shutdown = TRUE,
          task = uuid::UUIDgenerate()
        )
      )
      if (wait) {
        crew_wait(
          fun = function() {
            private$update(crashed = FALSE)
            !any(private$workers$up)
          },
          timeout = private$timeout,
          wait = private$wait
        )
      }
      invisible()
    }
  )
)
