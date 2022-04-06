# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
# Same as `queue_callr` except it uses the custom data store
#  and worker event loop.
queue <- R6::R6Class(
  classname = "queue",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    tasks = NULL,
    results = NULL,
    workers = NULL,
    store = NULL,
    timeout = NULL,
    wait = NULL,
    jobs = NULL,
    loop = NULL,
    initialize_tasks = function() {
      private$tasks <- tibble::tibble(
        task = character(0),
        fun = list(NULL),
        args = list(NULL)
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
        args = replicate(workers, NULL)
      )
    },
    add_task = function(task, fun, args) {
      dup <- task %in% private$tasks$task || task %in% private$workers$task
      crew_assert(!dup, paste("duplicate task name", task))
      args <- list(
        .data = private$tasks,
        task = task,
        fun = list(fun),
        args = list(args)
      )
      private$tasks <- do.call(what = tibble::add_row, args = args)
    },
    add_result = function(task, result) {
      private$results <- tibble::add_row(
        .data = private$results,
        task = task,
        result = list(result)
      )
    },
    update_tasks = function() {
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
    update_workers = function() {
      workers <- private$workers
      workers <- workers[order(workers$up, decreasing = TRUE),, drop = FALSE] # nolint
      workers <- workers[!workers$free & !workers$sent,, drop = FALSE] # nolint
      for (worker in workers$worker) {
        index <- which(private$workers$worker == worker)
        handle <- private$workers$handle[[index]]
        private$workers$handle[[index]] <- private$worker_run(
          handle = handle,
          worker = worker,
          fun = private$workers$fun[[index]],
          args = private$workers$args[[index]]
        )
        private$workers$sent[index] <- TRUE
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
        }
      }
    },
    update_done = function() {
      names <- private$store$list_worker_output()
      private$workers$done[private$workers$worker %in% names] <- TRUE
    },
    update_work = function() {
      private$update_crashed()
      private$update_done()
      private$update_results()
      private$update_tasks()
      private$update_workers()
    },
    update_crashed = function() {
      up <- map_lgl(private$workers$worker, private$worker_up_name)
      private$update_done()
      x <- private$workers
      crashed <- x$sent & !x$done & !up
      if (any(crashed)) {
        workers <- private$workers$worker[crashed]
        crew_error(paste("crashed workers:", paste(workers, collapse = ", ")))
      }
    },
    worker_run = function(handle, worker, fun, args) {
      task <- list(fun = deparse(fun), args = args)
      private$store$write_worker_input(worker = worker, value = task)
      if_any(
        private$worker_up_name(worker),
        handle,
        private$worker_start(worker)
      )
    },
    worker_start = function(worker) {
      callr::r_bg(
        func = function(worker, store, jobs, timeout, wait) {
          # executed inside the worker
          # nocov start
          crew::crew_worker(
            worker = worker,
            store = store,
            jobs = jobs,
            timeout = timeout,
            wait = wait
          )
          # nocov end
        },
        args = list(
          worker = worker,
          store = private$store$marshal(),
          jobs = private$jobs,
          timeout = private$timeout,
          wait = private$wait
        ),
        supervise = TRUE
      )
    },
    worker_up_name = function(worker) {
      index <- which(private$workers$worker == worker)
      handle <- private$workers$handle[[index]]
      up <- private$worker_up(handle)
      private$workers$up[index] <- up
      up
    },
    worker_up = function(handle) {
      !is.null(handle) && handle$is_alive()
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      store = store_local$new(timeout = timeout, wait = wait),
      timeout = 60,
      wait = 0.1,
      jobs = Inf
    ) {
      private$store <- store
      private$timeout <- timeout
      private$wait <- wait
      private$jobs <- jobs
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
        private$update_work()
      }
      invisible()
    },
    pop = function(update = TRUE) {
      if (update) {
        private$update_work()
      }
      results <- private$results
      out <- NULL
      if (nrow(results)) {
        out <- list(task = results$task[1], result = results$result[[1]])
        private$results <- private$results[-1, ]
      }
      out
    },
    update = function() {
      private$update_work()
    },
    crashed = function() {
      private$update_crashed()
    }
  )
)
