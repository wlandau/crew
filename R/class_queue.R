# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
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
    initialize_tasks = function() {
      private$tasks <- tibble::tibble(
        task = character(0),
        input = list()
      )
    },
    initialize_results = function() {
      private$results <- tibble::tibble(
        task = character(0),
        result = list()
      )
    },
    initialize_workers = function(workers) {
      private$workers <- tibble::tibble(
        worker = uuid::UUIDgenerate(n = workers),
        handle = replicate(workers, list(), simplify = FALSE),
        free = rep(TRUE, workers),
        sent = rep(FALSE, workers),
        up = rep(FALSE, workers),
        done = rep(FALSE, workers),
        task = rep(NA_character_, workers),
        input = replicate(workers, list(), simplify = FALSE)
      )
    },
    add_task = function(task, input) {
      dup <- task %in% private$tasks$task || task %in% private$workers$task
      crew_assert(!dup, paste("duplicate task name", task))
      args <- list(
        .data = private$tasks,
        task = task,
        input = list(input)
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
          task = private$workers$task[index],
          input = private$workers$input[[index]]
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
          private$worker_free(private$workers$worker[index])
        }
      }
    },
    update_done = function() {
      names <- private$store$list_worker_output()
      private$workers$done[private$workers$worker %in% names] <- TRUE
    },
    update_subqueue = function() {
    },
    update_all = function() {
      private$update_crashed()
      private$update_subqueue()
      private$update_done()
      private$update_results()
      private$update_tasks()
      private$update_workers()
    },
    update_crashed = function() {
      up <- map_lgl(private$workers$worker, private$worker_up_log)
      private$update_done()
      x <- private$workers
      crashed <- x$sent & !x$done & !up
      if (any(crashed)) {
        workers <- private$workers$worker[crashed]
        crew_error(paste("crashed workers:", paste(workers, collapse = ", ")))
      }
    },
    worker_run = function(handle, worker, task, input) {
      value <- list(fun = deparse(input$fun), args = input$args)
      private$store$write_worker_input(worker = worker, value = value)
      if_any(
        private$worker_up_log(worker),
        handle,
        private$worker_start(worker)
      )
    },
    worker_up_log = function(worker) {
      index <- which(private$workers$worker == worker)
      handle <- private$workers$handle[[index]]
      up <- private$worker_up(handle = handle, worker = worker)
      private$workers$up[index] <- up
      up
    },
    worker_reuse = function(handle) {
      handle
    },
    worker_free = function(worker) {
      index <- which(worker == private$workers$worker)
      private$workers$handle[[index]] <- private$worker_reuse(
        private$workers$handle[[index]]
      )
      private$workers$free[index] <- TRUE
      private$workers$sent[index] <- FALSE
      private$workers$done[index] <- FALSE
      private$workers$task[index] <- NA_character_
      private$workers$input[[index]] <- list()
    },
    available_wait = function(timeout = private$timeout, wait = private$wait) {
      crew_wait(
        private$available,
        timeout = timeout %|||% private$timeout,
        wait = wait %|||% private$wait,
        message = "timed out waiting for workers to be available."
      )
    },
    available = function() {
      occupied <- nrow(private$tasks) || !any(private$workers$free)
      if (occupied) {
        private$update_all()
      }
      !occupied
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
      update = TRUE,
      ...
    ) {
      fun <- rlang::as_function(fun)
      input <- list(fun = fun, args = args, ...)
      private$add_task(task = task, input = input)
      if (update) {
        private$update_all()
      }
      invisible()
    },
    pop = function(update = TRUE) {
      if (update) {
        private$update_all()
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
      private$update_all()
    },
    crashed = function() {
      private$update_crashed()
    },
    block = function(timeout = NULL, wait = NULL) {
      private$available_wait(timeout = timeout, wait = wait)
    },
    shutdown = function() {
      if (!is.null(private$subqueue$shutdown)) {
        private$subqueue$shutdown()
      }
    }
  )
)

queue_worker_start <- function(worker, store, jobs, timeout, wait) {
  crew::crew_worker(
    worker = worker,
    store = store,
    jobs = jobs,
    timeout = timeout,
    wait = wait
  )
}
