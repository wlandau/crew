#' @title Crew class.
#' @export
#' @aliases crew
#' @description `R6` class for a crew.
class_crew <- R6::R6Class(
  classname = "crew",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    #' @field name Character of length 1, crew name.
    name = NULL,
    #' @field store `R6` store object.
    store = NULL,
    #' @field worker_classes Named list of `R6ClassGenerator`
    #'   objects with worker definitions. The first one
    #'   is the default.
    worker_classes = NULL,
    #' @field workers Named list of worker objects.
    workers = NULL,
    #' @field max_workers Positive integer,
    #'   maximum number of workers in the crew.
    max_workers = NULL,
    #' @description Crew constructor.
    #' @param name Character of length 1, crew name.
    #' @param store `R6` store object.
    #' @param worker_classes List of `R6ClassGenerator`
    #'   objects (created with `R6Class`) with worker definitions
    #'   and constructor.
    #' @param max_workers Positive integer,
    #'   maximum number of workers in the crew.
    initialize = function(
      name = basename(tempfile(pattern = "crew_")),
      store = crew::class_store_local$new(),
      worker_classes = list(
        crew::class_worker_callr,
        crew::class_worker_future
      ),
      max_workers = 1
    ) {
      self$name <- name
      self$store <- store
      names(worker_classes) <- map_chr(worker_classes, ~.x$classname)
      self$worker_classes <- worker_classes
      self$workers <- list()
      self$max_workers <- max_workers
    },
    #' @description create worker objects from one or more worker definitions.
    #'   Does not actually launch the new worker objects.
    #' @param workers Number of new worker objects to create.
    #' @param class Name of the worker class to use for launching workers.
    #'   check the `worker_classes` field for possible names.
    #' @param ... Named arguments to the worker constructor.
    recruit = function(
      workers = 1L,
      class = names(self$worker_classes)[[1]],
      ...
    ) {
      crew_assert_pos_dbl_scalar(workers)
      crew_assert_chr_scalar(class)
      crew_assert(
        class %in% names(self$worker_classes),
        paste(
          "invalid worker class. Possible worker classes:",
          paste(names(self$worker_classes), collapse = ", ")
        )
      )
      for (index in seq_len(workers)) {
        worker <- self$worker_classes[[class]]$new(...)
        worker$crew <- self
        self$workers[[worker$name]] <- worker
      }
      invisible()
    },
    #' @description Launch all down workers in the crew.
    launch = function() {
      walk(self$workers, ~.x$launch())
    },
    #' @description Determine if any worker is unassigned
    #'   and ready to accept a new job.
    sendable = function() {
      
    },
    #' @description Send a job to an available worker. Assumes `sendable()`
    #'   on the crew returns `TRUE`.
    #' @param fun Function to run in the job.
    #' @param args Named list of arguments to `fun()`
    send = function(fun, args = list()) {
      crew_assert(is.function(fun))
      crew_assert(is.list(args))
      crew_assert_named(args)
      
    },
    #' @description Determine if any worker in the crew is done
    #'   with its current job and the job output is available for collection.
    receivable = function() {
      
    },
    #' @description Choose a receivable worker and collect its job output.
    receive = function() {
      
    },
    #' @description Shut down one or more running workers.
    #' @details This method loops through the workers
    #'   from first to last in the worker list. If the worker
    #'   is up (running) and sendable (able to receive a job)
    #'   then the worker is sent a shutdown command. Otherwise,
    #'   the loop moves on to another worker.
    #' @param workers Positive integer of length 1. Maximum number of workers
    #'   to try to shut down. Does not count running or non-sendable (busy)
    #'   workers.
    #' @param sendable_only Logical of length 1, whether to skip to another
    #'   worker to shut down if the current worker is not sendable.
    shutdown = function(workers = Inf, sendable_only = TRUE) {
      crew_assert_pos_dbl_scalar(workers)
      crew_assert_lgl_scalar(sendable_only)
      workers_shut_down <- 0
      for (worker in self$workers) {
        if ((!sendable_only || worker$sendable()) && worker$up()) {
          worker$shutdown()
          workers_shut_down <- workers_shut_down + 1
        }
        if (workers_shut_down >= workers) {
          break
        }
      }
      invisible()
    },
    #' @description delete all worker objects.
    #' @details This method loops through the workers
    #'   from first to last in the worker list. If the worker
    #'   is down and sendable (able to receive a job)
    #'   then the worker object is deleted from the crew. Otherwise,
    #'   the loop moves on to another worker.
    #' @param workers Positive integer of length 1. Maximum number of workers
    #'   to try to dismiss. Does not count down or non-sendable workers
    #'   (workers with unfinished jobs).
    #' @param sendable_only Logical of length 1, whether to skip to another
    #'   worker to dismiss if the current worker is not sendable.
    #' @param down_only Logical of length 1, whether to skip to another
    #'   worker to dismiss if the current worker is not down.
    dismiss = function(workers = Inf, sendable_only = TRUE, down_only = TRUE) {
      crew_assert_pos_dbl_scalar(workers)
      crew_assert_lgl_scalar(sendable_only)
      crew_assert_lgl_scalar(down_only)
      workers_dismissed <- 0
      check_these <- seq_along(self$workers)
      dismiss_these <- integer(0)
      for (index in check_these) {
        should_dismiss <- (!sendable_only || worker$sendable()) &&
          (!down_only || !worker$up())
        if (should_dismiss) {
          dismiss_these <- c(dismiss_these, index)
        }
        if (workers_dismissed >= workers) {
          break
        }
      }
      self$workers <- self$workers[dismiss_these]
      invisible()
    },
    #' @description Crew validator.
    validate = function() {
      crew_assert_chr_scalar(self$name, "crew has invalid name.")
      crew_assert(
        inherits(self$store, "store"),
        paste("invalid store in crew", self$name)
      )
      self$store$validate()
      crew_assert(length(self$worker_classes) > 0L)
      crew_assert(is.list(self$worker_classes))
      crew_assert_named(self$worker_classes)
      lapply(self$worker_classes, function(x) {
        crew_assert(inherits(x, "R6ClassGenerator"))
      })
      crew_assert(is.list(self$workers))
      crew_assert_named(self$workers)
      lapply(
        self$workers, function(x) {
          crew_assert(
            inherits(x, "worker"),
            paste(
              "In crew",
              self$crew,
              "not all elements of workers are workers."
            )
          )
        }
      )
      lapply(self$workers, function(x) x$validate())
      crew_assert_pos_dbl_scalar(self$max_workers)
    }
  )
)
