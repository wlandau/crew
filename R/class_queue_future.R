# Adapted from
#  <https://github.com/r-lib/callr/blob/811a02f604de2cf03264f6b35ce9ec8a412f2581/vignettes/taskq.R> # nolint
#  under the MIT license. See also the `crew` package `NOTICE` file.
queue_future <- R6::R6Class(
  classname = "queue_future",
  inherit = queue,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    plan = NULL,
    processes = NULL,
    subqueue = NULL,
    worker_run = function(handle, worker, fun, args) {
      private$subqueue_wait()
      task <- list(fun = deparse(fun), args = args)
      private$store$write_worker_input(worker = worker, value = task)
      fun <- function(worker, store, timeout, wait, plan) {
        # executed in the subqueue
        # nocov start
        plan_old <- future::plan()
        on.exit(future::plan(plan_old, .cleanup = FALSE))
        future::plan(plan, .cleanup = FALSE)
        future::future(
          expr = crew::crew_job(
            worker = worker,
            store = store,
            timeout = timeout,
            wait = wait
          ),
          substitute = TRUE,
          packages = character(0),
          globals = list(
            worker = worker,
            store = store,
            timeout = timeout,
            wait = wait
          ),
          lazy = FALSE,
          seed = TRUE
        )
        # nocov end
      }
      args <- list(
        worker = worker,
        store = private$store$marshal(),
        timeout = private$timeout,
        wait = private$wait,
        plan = private$plan
      )
      private$subqueue$push(fun = fun, args = args, task = worker)
      list(worker = worker)
    },
    worker_up = function(handle, worker) {
      
      browser()
      
      if (is.null(handle)) {
        return(FALSE)
      }
      if (length(handle) == 1L && is.logical(handle)) {
        return(handle)
      }
      private$subqueue_wait()
      private$subqueue$push(
        fun = function(future) !all(future::resolved(future)), # nocov
        args = list(future = handle),
        task = worker
      )
      TRUE
    },
    update_subqueue = function() {
      while(!is.null(result <- private$subqueue$pop())) {
        
        browser()
        
        if (!is.null(result$result$error)) {
          crew_error(result$result$error)
        }
        index <- which(private$workers$worker == result$task)
        private$workers$handle[[index]] <- result$result$result
      }
    },
    subqueue_wait = function() {
      crew_wait(
        private$subqueue_available,
        timeout = private$timeout,
        wait = private$wait,
        message = "timed out waiting for local processes to be available."
      )
    },
    subqueue_available = function() {
      subtasks <- private$subqueue$get_tasks()
      free <- private$subqueue$get_workers()$free
      out <- !nrow(subtasks) && any(free)
      if (!out) {
        private$subqueue$update()
      }
      out
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      processes = 1,
      jobs = Inf,
      timeout = 60,
      wait = 0.1,
      plan = future::plan()
    ) {
      super$initialize(
        workers = workers,
        jobs = jobs,
        timeout = timeout,
        wait = wait
      )
      private$processes <- processes
      private$subqueue <- queue_callr$new(
        workers = processes,
        wait = wait,
        timeout = timeout,
        start = TRUE
      )
      private$plan <- plan
      invisible()
    },
    get_plan = function() {
      private$plan
    }
  )
)
