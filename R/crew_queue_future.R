#' @title Future queue
#' @export
#' @aliases crew_queue_future
#' @family queue
#' @description Task queue with `future` workers.
#' @details The workers in this queue are futures with the `future` package.
#'   In `push()`, it is possible to supply different `future::plan()`s
#'   to different futures, and the queue itself has its own default plan.
#'   Plans must be serializable, so only the `sequential` and `future.batchtools`
#'   plans are supported.
#'   The queue has a subqueue from [crew_queue_session] (or [crew_queue_bg]
#'   if absolutely necessary) to submit and poll futures in order to
#'   eliminate the otherwise heavy overhead of `future::future()` and
#'   `future::resolved()` in `future.batchtools` futures. Tasks are
#'   sent and collected using a [crew_store_local] data store
#'   in order to eliminate the normally heavy overhead of `future::value()`.
#' @inheritSection crew_queue Queue attribution
#' @examples
#' fun <- function(x) x + 1
#' args <- list(x = 1)
#' queue <- crew_queue_future$new(timeout = 60, wait = 0.1)
#' queue$push(fun = fun, args = args)
#' queue$block()
#' result <- queue$pop()
#' str(result)
#' result$result$result
#' queue$shutdown()
#' processx::supervisor_kill()
crew_queue_future <- R6::R6Class(
  classname = "crew_queue_future",
  inherit = crew_queue,
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
        fun = crew_queue_future_worker_start,
        args = args,
        task = sprintf("%s|%s", worker, crew_name())
      )
      list(future = list(), task = task, resolved = FALSE)
    },
    worker_up = function(handle, worker) {
      if (all(private$workers$done[private$workers$worker == worker])) {
        return(TRUE)
      }
      if (!length(handle) || isTRUE(handle$resolved)) {
        return(FALSE)
      }
      if (!length(handle$future)) {
        return(TRUE)
      }
      private$subqueue$block()
      private$subqueue$push(
        fun = crew_queue_future_worker_resolve,
        args = list(
          handle = handle,
          worker = worker,
          store = private$store$marshal(),
          timeout = private$timeout
        ),
        task = sprintf("%s|%s", worker, crew_name())
      )
      TRUE
    },
    worker_reuse = function(handle) {
      list()
    },
    update_subqueue = function() {
      while (!is.null(result <- private$subqueue$pop())) {
        if (!is.null(result$result$error)) {
          if_any(
            is.character(result$result$error),
            crew_error(result$result$error),
            crew_error(conditionMessage(result$result$error))
          )
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
    #' @description Abstract queue constructor.
    #' @return An abstract queue object.
    #' @param workers Number of workers in the queue.
    #' @param store Data store object created with [crew_store_local]
    #'   or similar.
    #' @param timeout Number of seconds to for a worker to wait
    #'   for something to happen (e.g. the arrival of a task)
    #'   before timing out and quitting.
    #' @param wait Number of seconds to wait in between iterations while
    #'   waiting for something to happen (e.g. the arrival of a task).
    #' @param max_tasks Number of tasks a worker can run before quitting.
    #'   Some queues allow the worker to restart when launching a new task.
    #' @param plan A `future::plan()` object with the default plan.
    #' @param subqueue An object from [crew_queue_session] (or
    #'   [crew_queue_bg] if absolutely necessary) with the subqueue
    #'   which submits and collects futures in order to reduce overhead.
    initialize = function(
      workers = 1,
      store = crew_store_local$new(timeout = timeout, wait = wait),
      timeout = 60,
      wait = 0.1,
      max_tasks = Inf,
      plan = future::plan(),
      subqueue = crew_queue_session$new(
        workers = 1,
        wait = wait,
        timeout = timeout
      )
    ) {
      crew_true(inherits(plan, "future"))
      crew_true(
        inherits(subqueue, "crew_queue_session") ||
          inherits(subqueue, "crew_queue_bg")
      )
      super$initialize(
        workers = workers,
        store = store,
        timeout = timeout,
        wait = wait,
        max_tasks = max_tasks
      )
      private$plan <- plan
      private$subqueue <- subqueue
      invisible()
    },
    #' @description Get the default `future` plan of the queue.
    #' @return The default `future` plan object of the queue.
    get_plan = function() {
      private$plan
    },
    #' @description Push a new task on to the queue.
    #' @return `NULL` (invisibly)
    #' @param fun R function that runs the task.
    #' @param args Named list of arguments to `fun`.
    #' @param task Character of length 1 with the task ID. This ID
    #'   gets passed to the `label` object of `future::future()`
    #'   which shows up on `htop` and cluster viewing commands
    #'   such as `squeue` for SLURM and `qstat` for SGE.
    #' @param update Logical of length 1, whether to update the
    #'   internal state of the queue after pushing. See the
    #'   `update()` method for details.
    #' @param plan An optional `future` plan object for the task.
    push = function(
      fun,
      args = list(),
      task = crew_name(),
      update = TRUE,
      plan = NULL
    ) {
      if (!is.null(plan)) {
        crew_true(inherits(plan, "future"))
      }
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

crew_queue_future_worker_start <- function(
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
    expr = crew::crew_task(
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
  list(future = future, task = task)
}

crew_queue_future_worker_resolve <- function(handle, worker, store, timeout) {
  store <- eval(parse(text = store))
  resolved <- tryCatch(
    future::resolved(handle$future),
    error = crew::crew_condition_false
  )
  time <- Sys.time()
  if (resolved) {
    handle$time_resolved <- time
  }
  output <- tryCatch(
    store$exists_worker_output(worker),
    error = crew::crew_condition_false
  )
  if (resolved && !output) {
    if (!isTRUE(handle$checked_value)) {
      handle$value <- tryCatch(
        future::value(handle$future),
        error = crew::crew_condition_message
      )
      handle$checked_value <- TRUE
    }
    diff <- as.numeric(difftime(time, handle$time_resolved))
    if (isTRUE(any(diff > timeout))) {
      crew::crew_error("future worker ", worker, " crashed: ", handle$value)
    }
  }
  handle$resolved <- resolved && output
  handle
}
