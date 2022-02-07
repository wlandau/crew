#' @title Crew class.
#' @export
#' @aliases crew
#' @description `R6` class for a crew.
#' @details A crew object is an interface to manage multiple
#'   high-performance computing workers. Supported methods
#'   send jobs, receive output, and poll, etc.
#' @examples
#' crew <- class_crew$new()
#' crew$recruit(workers = 1, timeout = 1)
#' crew$send(fun = function(arg) paste("job", arg), args = list(arg = 1))
#' while (!crew$receivable()) Sys.sleep(0.1)
#' job <- crew$receive()
#' print(job$value)
#' print(job$error)
#' crew$shutdown()
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
    #' @description Crew constructor.
    #' @return The `new()` method calls the constructor
    #'   and returns a new crew object.
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
      )
    ) {
      self$name <- name
      self$store <- store
      names(worker_classes) <- map_chr(worker_classes, ~.x$classname)
      self$worker_classes <- worker_classes
      self$workers <- list()
    },
    #' @description create worker objects from one or more worker definitions.
    #'   Does not actually launch the new worker objects.
    #' @return `NULL` (invisibly).
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
      new_workers <- lapply(
        seq_len(workers),
        function(index) self$worker_classes[[class]]$new(...)
      )
      walk(new_workers, function(x) x$crew <- self)
      names(new_workers) <- map_chr(new_workers, ~.x$name)
      self$workers <- c(self$workers, new_workers)
      invisible()
    },
    #' @description Launch all down workers in the crew.
    #' @return `NULL` (invisibly).
    #' @param tags Character vector of allowable tags of eligible workers.
    launch = function(tags = NULL) {
      if (!is.null(tags)) {
        workers <- fltr(self$workers, ~.x$tagged(tags))
      }
      walk(workers, ~.x$launch())
      invisible()
    },
    #' @description Determine if any worker is unassigned
    #'   and ready to accept a new job.
    #' @return `TRUE` if the worker can accept a job and `FALSE` otherwise.
    #' @param tags Character vector of allowable tags of eligible workers.
    sendable = function(tags = NULL) {
      crew_assert(is.null(tags) || is.character(tags))
      for (worker in self$workers) {
        if (worker$sendable() && (is.null(tags) || worker$tagged(tags))) {
          return(TRUE)
        }
      }
      FALSE
    },
    #' @description Send a job to an available worker. Assumes `sendable()`
    #'   on the crew returns `TRUE`.
    #' @return `NULL` (invisibly).
    #' @param fun Function to run in the job.
    #' @param args Named list of function arguments to `fun`.
    #' @param tags Character vector of allowable tags of eligible workers.
    send = function(
      fun,
      args = list(),
      tags = NULL
    ) {
      crew_assert(is.function(fun))
      crew_assert(is.list(args))
      crew_assert_named(args)
      # Try already up workers first.
      for (worker in self$workers) {
        eligible <- worker$sendable() &&
          (is.null(tags) || worker$tagged(tags)) &&
          worker$up()
        if (eligible) {
          worker$send(fun = fun, args = args)
          return(invisible())
        }
      }
      # If now workers are up, loop again and launch if needed.
      for (worker in self$workers) {
        if (worker$sendable() && (is.null(tags) || worker$tagged(tags))) {
          worker$send(fun = fun, args = args)
          return(invisible())
        }
      }
      crew_error(
        paste(
          "all eligible workers in crew",
          self$name,
          "are either nonexistent or busy and cannot accept jobs."
        )
      )
    },
    #' @description Determine if any worker in the crew is done
    #'   with its current job and the job output is available for collection.
    #' @return `TRUE` if there exists an eligible worker with job output
    #'   that can be accessed with `receive()`. `FALSE` otherwise.
    #' @param tags Character vector of allowable tags of eligible workers.
    receivable = function(tags = NULL) {
      for (worker in self$workers) {
        if (worker$receivable() && (is.null(tags) || worker$tagged(tags))) {
          return(TRUE)
        }
      }
      FALSE
    },
    #' @description Find a receivable worker, collect its job output,
    #'   and free up the worker.
    #' @details Once collected and returned. the job output is deleted from
    #'   the data store and no longer available to receive.
    #'   `receive()` also marks the worker as "sendable" again (unblocked)
    #'   which makes the worker ready for another job (`send()` method).
    #' @return A named list of job output. The `value` element has the
    #'   actual result of the job function, if successful. Other elements
    #'   have job metadata such as the error message (if any), traceback,
    #'   warnings, and runtime in seconds.
    #' @param tags Character vector of allowable tags of eligible workers.
    receive = function(tags = NULL) {
      for (worker in self$workers) {
        if (worker$receivable() && (is.null(tags) || worker$tagged(tags))) {
          return(worker$receive())
        }
      }
      crew_error(paste("no job output from crew", self$name))
    },
    #' @description Clear worker input and output.
    #' @details Deletes worker input and output files.
    #' @return `NULL` (invisibly)
    #' @param timeout Number of seconds to wait for the file deletion to
    #'   succeed.
    #' @param wait Number of seconds to wait between iterations checking
    #'   that the worker files were successfully removed from the data store.
    #' @param down_only Logical of length 1, whether to only clear down jobs.
    #' @param tags Character vector of allowable tags of eligible workers.
    clear = function(timeout = 60, wait = 1, down_only = TRUE, tags = NULL) {
      for (worker in self$workers) {
        eligible <- (!down_only || !worker$up()) &&
          (is.null(tags) || worker$tagged(tags))
        if (eligible) {
          worker$clear(timeout = timeout, wait = wait)
        }
      }
      invisible()
    },
    #' @description Shut down one or more running workers.
    #' @details This method loops through the workers
    #'   from first to last in the worker list. If the worker
    #'   is up (running) and sendable (able to receive a job)
    #'   then the worker is sent a shutdown command. Otherwise,
    #'   the loop moves on to another worker.
    #' @return `NULL` (invisibly).
    #' @param workers Positive integer of length 1. Maximum number of workers
    #'   to try to shut down. Does not count busy (non-sendable)
    #'   workers.
    #' @param sendable_only Logical of length 1, whether to ignore
    #'   workers that are not sendable.
    #' @param up_only Logical of length 1, whether to ignore workers
    #'   that are not up.
    #' @param tags Character vector of allowable tags of eligible workers.
    shutdown = function(
      workers = Inf,
      sendable_only = TRUE,
      up_only = TRUE,
      tags = NULL
    ) {
      crew_assert_pos_dbl_scalar(workers)
      crew_assert_lgl_scalar(sendable_only)
      crew_assert_lgl_scalar(up_only)
      workers_shut_down <- 0
      for (worker in self$workers) {
        eligible <- (!sendable_only || worker$sendable()) &&
          (!up_only || worker$up()) &&
          (is.null(tags) || worker$tagged(tags))
        if (eligible) {
          worker$shutdown()
          workers_shut_down <- workers_shut_down + 1
        }
        if (workers_shut_down >= workers) {
          break
        }
      }
      invisible()
    },
    #' @description Delete one or more worker objects.
    #' @details This method loops through the workers
    #'   from first to last in the worker list. If the worker
    #'   is down and sendable (able to receive a job)
    #'   then the worker object is deleted from the crew. Otherwise,
    #'   the loop moves on to another worker.
    #' @return `NULL` (invisibly).
    #' @param workers Positive integer of length 1. Maximum number of workers
    #'   to try to dismiss. Does not count up or busy (non-sendable) workers
    #'   (workers with unfinished jobs).
    #' @param sendable_only Logical of length 1, whether to ignore
    #'   workers that are not sendable.
    #' @param down_only Logical of length 1, whether to ignore
    #'   workers that are still up.
    #' @param tags Character vector of allowable tags of eligible workers.
    dismiss = function(
      workers = Inf,
      sendable_only = TRUE,
      down_only = TRUE,
      tags = NULL
    ) {
      crew_assert_pos_dbl_scalar(workers)
      crew_assert_lgl_scalar(sendable_only)
      crew_assert_lgl_scalar(down_only)
      check_these <- seq_along(self$workers)
      dismiss_these <- integer(0)
      for (index in check_these) {
        worker <- self$workers[[index]]
        eligible <- (!sendable_only || worker$sendable()) &&
          (!down_only || !worker$up()) &&
          (is.null(tags) || worker$tagged(tags))
        if (eligible) {
          dismiss_these <- c(dismiss_these, index)
        }
        if (length(dismiss_these) >= workers) {
          break
        }
      }
      for (index in sort(dismiss_these, decreasing = TRUE)) {
        self$workers[[index]] <- NULL
      }
      invisible()
    },
    #' @description Restart one or more stuck workers.
    #' @details This method loops through the workers
    #'   from first to last in the worker list. If the worker
    #'   is stuck, then the worker is restarted. Otherwise,
    #'   the loop moves on to another worker.
    #' @return `NULL` (invisibly).
    #' @param workers Positive integer of length 1. Maximum number of workers
    #'   to try to restart. Does not count already unstuck workers.
    #' @param tags Character vector of allowable tags of eligible workers.
    restart = function(workers = Inf, tags = NULL) {
      crew_assert_pos_dbl_scalar(workers)
      workers_unstick <- 0
      for (worker in self$workers) {
        if ((is.null(tags) || worker$tagged(tags)) && worker$stuck()) {
          worker$restart()
          workers_unstick <- workers_unstick + 1
        }
        if (workers_unstick >= workers) {
          break
        }
      }
      invisible()
    },
    #' @description Crew validator.
    #' @return `NULL` (invisibly).
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
      invisible()
    }
  )
)
