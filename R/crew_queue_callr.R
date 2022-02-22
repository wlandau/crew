# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
crew_queue_callr <- R6::R6Class(
  classname = "crew_queue_callr",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    tasks = NULL,
    results = NULL,
    workers = NULL,
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
    initialize_workers = function(workers, start) {
      handles <- if_any(
        start,
        replicate(
          workers,
          callr::r_session$new(
            wait = TRUE,
            options = callr::r_session_options(extra = list(supervise = TRUE))
          )
        ),
        replicate(workers, NULL)
      )
      private$workers <- tibble::tibble(
        worker = uuid::UUIDgenerate(n = workers),
        handle = handles,
        free = rep(TRUE, workers),
        sent = rep(FALSE, workers),
        done = rep(FALSE, workers),
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
    send_task = function(worker, fun, args) {
      index <- which(private$workers$worker == worker)
      handle <- private$workers$handle[[index]]
      handle$call(func = fun, args = args)
      private$workers$sent[index] <- TRUE
    },
    poll_done = function() {
      index <- which(private$workers$sent)
      handles <- private$workers$handle[index]
      connections <- map(handles, ~.x$get_poll_connection())
      poll <- as.character(processx::poll(processes = connections, ms = 0))
      crew_assert(!any(poll == "closed"), "a callr worker is down.")
      private$workers$done[index] <- poll == "ready"
    },
    poll = function() {
      handles <- private$workers$handle
      connections <- map(handles, ~.x$get_poll_connection())
      as.character(processx::poll(processes = connections, ms = 0))
    },
    receive_results = function() {
      for (index in seq_len(nrow(private$workers))) {
        if (private$workers$done[index]) {
          task <- private$workers$task[index]
          result <- private$workers$handle[[index]]$read()
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
    initialize = function(workers = 1, start = TRUE) {
      private$initialize_tasks()
      private$initialize_results()
      private$initialize_workers(workers = workers, start = start)
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
        if (!is.null(handle) && handle$is_alive()) {
          handle$kill()
        }
      }
      invisible()
    }
  )
)
