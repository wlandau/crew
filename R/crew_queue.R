# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
crew_queue <- R6::R6Class(
  classname = "crew_queue",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    tasks = NULL,
    workers = NULL,
    initialize_tasks = function() {
      private$tasks <- tibble::tibble(
        task = character(0),
        tags = list(NULL),
        fun = list(NULL),
        args = list(NULL)
      )
      invisible()
    },
    initialize_workers = function() {
      private$workers <- tibble::tibble(
        worker = character(0),
        timeout = numeric(0),
        wait = numeric(0),
        tags = list(NULL),
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
      invisible()
    },
    add_task = function(fun, args, tags) {
      private$tasks <- tibble::add_row(
        .data = private$tasks,
        task = uuid::UUIDgenerate(),
        tags = list(tags),
        fun = list(fun),
        args = list(args)
      )
      invisible()
    },
    add_worker = function(timeout, wait, tags) {
      private$workers <- tibble::add_row(
        .data = private$workers,
        worker = uuid::UUIDgenerate(),
        timeout = timeout,
        wait = wait,
        tags = list(tags),
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
      invisible()
    },
    remove_task = function(task) {
      private$tasks <- private$tasks[private$tasks$task != task, ]
    },
    remove_worker = function(worker) {
      private$workers <- private$workers[private$workers$worker != worker, ]
    }
  ),
  public = list(
    initialize = function() {
      private$initialize_workers()
      private$initialize_tasks()
      invisible()
    },
    get_tasks = function() {
      private$tasks
    },
    get_workers = function() {
      private$workers
    },
    add_workers = function(
      workers = 1,
      timeout = Inf,
      wait = 0,
      tags = character(0)
    ) {
      walk(
        seq_len(workers),
        ~private$add_worker(
          timeout = timeout,
          wait = wait,
          tags = tags
        )
      )
      invisible()
    }
  )
)
