# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
crew_queue_sync <- R6::R6Class(
  classname = "crew_queue_sync",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    timeout = NULL,
    wait = NULL,
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
    initialize_workers = function() {
      private$workers <- tibble::tibble(
        worker = character(0),
        handle = list(NULL),
        up = logical(0),
        free = logical(0),
        sent = logical(0),
        done = logical(0),
        task = character(0),
        fun = list(NULL),
        args = list(NULL)
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
    add_worker = function() {
      private$workers <- tibble::add_row(
        .data = private$workers,
        worker = uuid::UUIDgenerate(),
        handle = list(NULL),
        up = FALSE,
        free = TRUE,
        sent = FALSE,
        done = FALSE,
        task = NA_character_,
        fun = list(NULL),
        args = list(NULL)
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
      if (is.null(handle) || !handle$is_alive()) {
        handle <- callr::r_session$new(wait = TRUE)
        private$workers$handle[[index]] <- handle
      }
      handle$call(func = fun, args = args)
      private$workers$sent[index] <- TRUE
    },
    poll_up = function() {
      for (index in seq_len(nrow(private$workers))) {
        handle <- private$workers$handle[[index]]
        private$workers$up[index] <- !is.null(handle) && handle$is_alive()
      }
    },
    poll_done = function() {
      for (index in seq_len(nrow(private$workers))) {
        handle <- private$workers$handle[[index]]
        private$workers$done[index] <- !is.null(handle) && identical(
          processx::poll(list(handle$get_poll_connection()), 0)[[1]],
          "ready"
        )
      }
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
    initialize = function(timeout = Inf, wait = 0) {
      private$timeout <- timeout
      private$wait <- wait
      private$initialize_tasks()
      private$initialize_results()
      private$initialize_workers()
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
    add_workers = function(workers = 1) {
      replicate(workers, private$add_worker())
      invisible()
    },
    remove_workers = function() {
      private$poll_up()
      free <- private$workers$free
      up <- private$workers$up
      private$workers <- private$workers[!free | up, ]
      invisible()
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
    }
  )
)
