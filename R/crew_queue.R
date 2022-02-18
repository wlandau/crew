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
    workers = NULL,
    initialize_tasks = function() {
      private$tasks <- tibble::tibble(
        task = character(0),
        fun = list(NULL),
        args = list(NULL)
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
    add_task = function(fun, args) {
      private$tasks <- tibble::add_row(
        .data = private$tasks,
        task = uuid::UUIDgenerate(),
        fun = list(fun),
        args = list(args)
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
        for (field in colnames(private$tasks$task)) {
          value <- private$tasks[[field]]
          private$workers[[field]][index] <- value
        }
        for (field in c("free", "sent", "done")) {
          private$workers$[[field]][index] <- FALSE
        }
        private$tasks <- private$tasks[-1, ]
      }
    }
  ),
  public = list(
    initialize = function(timeout = Inf, wait = 0) {
      private$timeout <- timeout
      private$wait <- wait
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
    add_workers = function(workers = 1) {
      walk(seq_len(workers), private$add_worker)
    }
    prune_workers = function() {
      free <- private$workers$free
      up <- private$workers$up
      lock <- private$workers$lock
      private$workers <- private$workers[free & !up & !lock, ]
      invisible()
    }
  )
)
