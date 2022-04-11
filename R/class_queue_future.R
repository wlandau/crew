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
      private$subqueue$block()
      task <- list(fun = deparse(fun), args = args)
      private$store$write_worker_input(worker = worker, value = task)
      args <- list(
        worker = worker,
        store = private$store$marshal(),
        timeout = private$timeout,
        wait = private$wait,
        plan = private$plan
      )
      private$subqueue$push(
        fun = queue_future_worker_launch,
        args = args,
        task = worker
      )
      list(future = NULL, resolved = FALSE)
    },
    worker_up = function(handle, worker) {
      
      browser()
      
      if (is.null(handle$future)) {
        return(FALSE)
      }
      if (handle$resolved) {
        return(TRUE)
      }
      private$subqueue$block()
      private$subqueue$push(
        fun = queue_future_worker_resolve,
        args = list(future = handle$future),
        task = worker
      )
      TRUE
    },
    update_subqueue = function() {
      
      browser()
      
      while(!is.null(result <- private$subqueue$pop())) {
        
        
        
        if (!is.null(result$result$error)) {
          crew_error(result$result$error)
        }
        index <- which(private$workers$worker == result$task)
        private$workers$handle[[index]] <- result$result$result
      }
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

queue_future_worker_launch <- function(worker, store, timeout, wait, plan) {
  plan_old <- future::plan()
  on.exit(future::plan(plan_old, .cleanup = FALSE))
  future::plan(plan, .cleanup = FALSE)
  future <- future::future(
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
  list(future = future, resolved = FALSE)
}

queue_future_worker_resolve <- function(future) {
  list(future = future, resolved = future::resolved(future))
}
