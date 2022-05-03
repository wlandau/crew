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
    subqueue = NULL,
    worker_run = function(handle, worker, task, input) {
      private$subqueue$block()
      value <- list(fun = deparse(input$fun), args = input$args)
      private$store$write_worker_input(worker = worker, value = value)
      args <- list(
        worker = worker,
        store = private$store$marshal(),
        timeout = private$timeout,
        wait = private$wait,
        plan = input$plan %|||% private$plan,
        task = task
      )
      private$subqueue$push(
        fun = queue_future_worker_start,
        args = args,
        task = sprintf("%s|%s", worker, uuid::UUIDgenerate())
      )
      list(future = list(), task = task, resolved = FALSE)
    },
    worker_up = function(handle, worker) {
      if (!length(handle) || isTRUE(handle$resolved)) {
        return(FALSE)
      }
      if (!length(handle$future)) {
        return(TRUE)
      }
      private$subqueue$block()
      private$subqueue$push(
        fun = queue_future_worker_resolve,
        args = list(handle = handle),
        task = sprintf("%s|%s", worker, uuid::UUIDgenerate())
      )
      TRUE
    },
    worker_reuse = function(handle) {
      list()
    },
    update_subqueue = function() {
      while (!is.null(result <- private$subqueue$pop())) {
        if (!is.null(result$result$error)) {
          crew_error(conditionMessage(result$result$error))
        }
        worker <- gsub("\\|.*$", "", result$task)
        index <- which(private$workers$worker == worker)
        if (length(index) == 1L) {
          task <- private$workers$task[index]
          if (identical(result$result$result$task, task)) {
            private$workers$handle[[index]] <- result$result$result
          }
        }
      }
    }
  ),
  public = list(
    initialize = function(
      workers = 1,
      store = store_local$new(timeout = timeout, wait = wait),
      timeout = 60,
      wait = 0.1,
      jobs = Inf,
      plan = future::plan(),
      processes = 1,
      subqueue = queue_session$new(
        workers = processes,
        wait = wait,
        timeout = timeout
      )
    ) {
      super$initialize(
        workers = workers,
        store = store,
        timeout = timeout,
        wait = wait,
        jobs = jobs
      )
      private$plan <- plan
      private$subqueue <- subqueue
      invisible()
    },
    get_plan = function() {
      private$plan
    },
    push = function(
      fun,
      args = list(),
      task = uuid::UUIDgenerate(),
      update = TRUE,
      plan = NULL
    ) {
      super$push(
        fun = fun,
        args = args,
        task = task,
        update = update,
        plan = plan
      )
    }
  )
)

queue_future_worker_start <- function(
  worker,
  store,
  timeout,
  wait,
  plan,
  task
) {
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
    globals = list(
      worker = worker,
      store = store,
      timeout = timeout,
      wait = wait
    ),
    label = task,
    substitute = TRUE,
    packages = character(0),
    lazy = FALSE,
    seed = TRUE
  )
  list(future = future, task = task, resolved = FALSE)
}

queue_future_worker_resolve <- function(handle) {
  handle$resolved <- future::resolved(handle$future)
  handle
}
