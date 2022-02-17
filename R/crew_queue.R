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
    insert_task = function(fun, args, tags) {
      private$tasks <- tibble::add_row(
        .data = private$tasks,
        task = uuid::UUIDgenerate(),
        tags = list(tags),
        fun = list(fun),
        args = list(args)
      )
      invisible()
    },
    insert_worker = function(tags) {
      private$workers <- tibble::add_row(
        .data = private$workers,
        worker = uuid::UUIDgenerate(),
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
    add_task = function(fun, args = list(), tags = character(0)) {
      private$insert_task(fun = fun, args = args, tags = tags)
    },
    add_workers = function(workers = 1, tags = character(0)) {
      walk(seq_len(workers), ~private$insert_worker(tags = tags))
    },
    remove_task = function(task) {
      private$tasks <- private$tasks[private$tasks$task != task, ]
    },
    remove_worker = function(worker) {
      private$workers <- private$workers[private$workers$worker != worker, ]
    }
  )
)
