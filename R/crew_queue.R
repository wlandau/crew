# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
crew_queue <- R6::R6Class(
  classname = "crew_queue",
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
      private$tasks <- tibble::tibble(
        task = character(0),
        result = list(NULL)
      )
    },
    initialize_workers = function() {
      private$workers <- tibble::tibble(
        worker = character(0),
        handle = list(NULL),
        up = logical(0),
        lock = logical(0),
        free = logical(0),
        sent = logical(0),
        done = logical(0),
        task = character(0),
        fun = list(NULL),
        args = list(NULL),
        result = list(NULL)
      )
    },
    add_task = function(task, fun, args) {
      
      browser()
      
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
        results = list(result)
      )
    },
    add_worker = function() {
      private$workers <- tibble::add_row(
        .data = private$workers,
        worker = uuid::UUIDgenerate(),
        handle = list(NULL),
        up = FALSE,
        lock = FALSE,
        free = TRUE,
        sent = FALSE,
        done = FALSE,
        task = NA_character_,
        fun = list(NULL),
        args = list(NULL),
        result = list(NULL)
      )
    },
    assign_tasks = function() {
      while(nrow(private$tasks) && any(private$workers$free)) {
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
      which <- !workers$free & !workers$lock & !workers$sent
      workers <- workers[which, ]
      for (worker in workers$worker) {
        index <- which(private$workers$worker == worker)
        private$workers$lock[index] <- TRUE
        private$send(
          worker = worker,
          fun = private$workers$fun[[index]],
          args = private$workers$args[[index]]
        )
      }
    },
    launch = function() {
      callr::r_session$new(wait = TRUE)
    },
    send = function(worker, fun, args) {
      index <- which(private$workers$worker == worker)
      handle <- private$workers$handle[[index]]
      if (is.null(handle) || !handle$is_alive()) {
        handle <- private$launch()
        private$workers$handle[[index]] <- handle
      }
      handle$call(func = fun, args = args)
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
    receive = function() {
      for (index in seq_len(nrow(private$workers))) {
        if (private$workers$done[index]) {
          task <- private$workers$task[index]
          result <- private$workers$handle[[index]]$read()
          private$add_result(task = task, result = result)
        }
      }
    },
    pop_result = function() {
      results <- private$results
      out <- NULL
      if (nrow(results)) {
        out <- list(task = results$tasks[1], result = results$result[[1]])
        private$results <- private$results[-1, ]
      }
      out
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
    get_workers = function() {
      private$workers
    },
    scale_out = function(workers = 1) {
      replicate(workers, private$add_worker())
      invisible()
    },
    scale_back = function() {
      free <- private$workers$free
      up <- private$workers$up
      lock <- private$workers$lock
      private$workers <- private$workers[!free | up | lock, ]
      invisible()
    },
    push = function(fun, args, task = uuid::UUIDgenerate()) {
      private$push_task(fun = fun, args = args, task = task)
      invisible()
    }
    pop = function() {
      private$pop_task()
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
