# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
# TODO:
# 0. poll_done() should just be poll() everywhere
# 1. Debug push and pop
# 2. add a synchronous callr queue as a field.
# 3. write directly to main input and main output, and make the sync queue upload/download. collect the result asynchronously.
# 4. make the sync queue poll for done workers. collect the result asynchronously. 
crew_queue_callr_async <- R6::R6Class(
  classname = "crew_queue_callr_async",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    tasks = NULL,
    results = NULL,
    workers = NULL,
    store = NULL,
    timeout = NULL,
    wait = NULL,
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
      private$tasks <- tibble::add_row(
        .data = private$tasks,
        task = task,
        fun = list(fun),
        args = list(args)
      )
    },
    add_result = function(task, result) {
      private$results <- tibble::add_row(
        .data = private$results,
        task = task,
        result = list(result)
      )
    },
    assign_tasks = function() {
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
    send_tasks = function() {
      workers <- private$workers
      which <- !workers$free & !workers$sent
      workers <- workers[which, ]
      for (worker in workers$worker) {
        index <- which(private$workers$worker == worker)
        private$send_task(
          worker = worker,
          fun = private$workers$fun[[index]],
          args = private$workers$args[[index]]
        )
      }
    },
    launch_worker = function(worker) {
      handle <- callr::r_bg(
        func = crew::crew_worker_loop,
        args = list(
          worker = worker,
          store = private$store$marshal(),
          timeout = private$timeout,
          wait = private$wait
        ),
        supervise = TRUE
      )
      crew_wait(
        fun = function(handle) handle$is_alive(),
        args = list(handle = handle),
        timeout = private$timeout,
        wait = private$wait
      )
      handle
    },
    send_task = function(worker, fun, args) {
      index <- which(private$workers$worker == worker)
      handle <- private$workers$handle[[index]]
      if (is.null(handle) || !handle$is_alive()) {
        private$workers$handle[[index]] <- private$launch_worker(worker)
      }
      task <- structure(
        list(fun = deparse(fun), args = args),
        class = "crew_task"
      )
      private$store$write_worker_input(
        worker = worker,
        value = task
      )
      private$workers$sent[index] <- TRUE
    },
    poll_done = function() {
      names <- private$store$list_worker_output()
      private$workers$done[private$workers$worker %in% names] <- TRUE
    },
    receive_results = function() {
      for (index in seq_len(nrow(private$workers))) {
        if (private$workers$done[index]) {
          worker <- private$workers$worker[index]
          task <- private$workers$task[index]
          result <- private$store$read_worker_output(worker = worker)
          private$add_result(task = task, result = result)
          private$workers$free[index] <- TRUE
          private$workers$sent[index] <- FALSE
          private$workers$done[index] <- FALSE
        }
      }
    },
    pop_result = function() {
      results <- private$results
      out <- NULL
      if (nrow(results)) {
        out <- list(task = results$task[1], result = results$result[[1]])
        private$results <- private$results[-1, ]
      }
      out
    },
    update_tasks = function() {
      private$poll_done()
      private$receive_results()
      private$assign_tasks()
      private$send_tasks()
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      timeout = 60,
      wait = 0.1,
      store = crew_store_local$new(timeout = timeout, wait = wait)
    ) {
      private$store <- store
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
    push = function(fun, args, task = uuid::UUIDgenerate()) {
      fun <- rlang::as_function(fun)
      private$add_task(fun = fun, args = args, task = task)
      private$update_tasks()
      invisible()
    },
    pop = function() {
      private$update_tasks()
      private$pop_result()
    },
    shutdown = function() {
      for (handle in private$workers$handle) {
        if (!is.null(handle)) {
          handle$kill()
        }
      }
      invisible()
    }
  )
)
