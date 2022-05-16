#' @title Abstract queue
#' @export
#' @aliases crew_queue
#' @family queue
#' @description Abstract class for a task queue.
#' @details This is an abstract class and not runnable by itself.
#'   Please instantiate using a subclass such as [crew_queue_session].
#' @section Queue attribution:
#'   The design and implementation of `crew` task queues, especially
#'   the local `callr`-based task queues, borrows heavily from
#'   the work of Gábor Csárdi, particularly
#'   <https://www.tidyverse.org/blog/2019/09/callr-task-q/>,
#'   which was also released in commit
#'   `811a02f604de2cf03264f6b35ce9ec8a412f2581`
#'   of <https://github.com/r-lib/callr> in the
#'   `vignettes/Task-queue-with-callr.Rmd`
#'   file under the MIT license. See the `crew` package `README.md` and
#'   `NOTICE` files for other mentions of this attribution.
#' @examples
#' fun <- function(x) x + 1
#' args <- list(x = 1)
#' queue <- crew_queue_session$new(timeout = 60, wait = 0.1)
#' queue$push(fun = fun, args = args)
#' queue$block()
#' result <- queue$pop()
#' str(result)
#' result$result$result
#' queue$shutdown()
#' processx::supervisor_kill()
crew_queue <- R6::R6Class(
  classname = "crew_queue",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    tasks = NULL,
    results = NULL,
    workers = NULL,
    store = NULL,
    timeout = NULL,
    wait = NULL,
    max_tasks = NULL,
    initialize_tasks = function() {
      private$tasks <- tibble::tibble(
        task = character(0),
        input = list()
      )
    },
    initialize_results = function() {
      private$results <- tibble::tibble(
        task = character(0),
        result = list()
      )
    },
    initialize_workers = function(workers) {
      private$workers <- tibble::tibble(
        worker = crew_name(n = workers),
        handle = replicate(workers, list(), simplify = FALSE),
        free = rep(TRUE, workers),
        sent = rep(FALSE, workers),
        up = rep(FALSE, workers),
        done = rep(FALSE, workers),
        task = rep(NA_character_, workers),
        input = replicate(workers, list(), simplify = FALSE)
      )
    },
    add_task = function(task, input) {
      dup <- task %in% private$tasks$task || task %in% private$workers$task
      crew_true(!dup, message = paste("duplicate task name", task))
      args <- list(
        .data = private$tasks,
        task = task,
        input = list(input)
      )
      private$tasks <- do.call(what = tibble::add_row, args = args)
    },
    add_result = function(task, result) {
      private$results <- tibble::add_row(
        .data = private$results,
        task = task,
        result = list(result)
      )
    },
    update_tasks = function() {
      while (nrow(private$tasks) && any(private$workers$free)) {
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
    update_workers = function() {
      workers <- private$workers
      workers <- workers[order(workers$up, decreasing = TRUE),, drop = FALSE] # nolint
      workers <- workers[!workers$free & !workers$sent,, drop = FALSE] # nolint
      for (worker in workers$worker) {
        index <- which(private$workers$worker == worker)
        handle <- private$workers$handle[[index]]
        private$workers$handle[[index]] <- private$worker_run(
          handle = handle,
          worker = worker,
          task = private$workers$task[index],
          input = private$workers$input[[index]]
        )
        private$workers$sent[index] <- TRUE
      }
    },
    update_results = function() {
      for (index in seq_len(nrow(private$workers))) {
        if (private$workers$done[index]) {
          worker <- private$workers$worker[index]
          task <- private$workers$task[index]
          result <- private$store$read_output(worker = worker)
          private$store$delete_output(worker = worker)
          private$add_result(task = task, result = result)
          private$worker_free(private$workers$worker[index])
        }
      }
    },
    update_done = function() {
      output <- private$store$list_output()
      sent <- private$workers$worker[private$workers$sent]
      names <- intersect(output, sent)
      private$workers$done[private$workers$worker %in% names] <- TRUE
    },
    update_subqueue = function() {
    },
    update_all = function() {
      private$update_crashed()
      private$update_subqueue()
      private$update_done()
      private$update_results()
      private$update_tasks()
      private$update_workers()
    },
    update_crashed = function() {
      workers <- private$workers
      index <- workers$sent & !workers$done
      workers <- workers[index,, drop = FALSE] # nolint
      up <- map_lgl(workers$worker, private$worker_up_log)
      private$update_done()
      workers <- private$workers[index,, drop = FALSE] # nolint
      crashed <- workers$sent & !workers$done & !up
      if (any(crashed)) {
        workers <- private$workers$worker[crashed]
        crew_error(paste("crashed workers:", paste(workers, collapse = ", ")))
      }
    },
    worker_run = function(handle, worker, task, input) {
      value <- list(fun = deparse(input$fun), args = input$args, task = task)
      private$store$write_input(worker = worker, value = value)
      if_any(
        private$worker_up_log(worker),
        handle,
        private$worker_start(worker)
      )
    },
    worker_up_log = function(worker) {
      index <- which(private$workers$worker == worker)
      handle <- private$workers$handle[[index]]
      up <- private$worker_up(handle = handle, worker = worker)
      private$workers$up[index] <- up
      up
    },
    worker_reuse = function(handle) {
      handle
    },
    worker_free = function(worker) {
      index <- which(worker == private$workers$worker)
      private$workers$handle[[index]] <- private$worker_reuse(
        private$workers$handle[[index]]
      )
      private$workers$free[index] <- TRUE
      private$workers$sent[index] <- FALSE
      private$workers$done[index] <- FALSE
      private$workers$task[index] <- NA_character_
      private$workers$input[[index]] <- list()
    },
    available_wait = function(timeout = private$timeout, wait = private$wait) {
      crew_wait(
        private$available,
        timeout = timeout %|||% private$timeout,
        wait = wait %|||% private$wait,
        message = "timed out waiting for workers to be available."
      )
    },
    available = function() {
      occupied <- nrow(private$tasks) || !any(private$workers$free)
      if (occupied) {
        private$update_all()
      }
      !occupied
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
    initialize = function(
      workers = 1,
      store = crew_store_local$new(timeout = timeout, wait = wait),
      timeout = 60,
      wait = 0.1,
      max_tasks = Inf
    ) {
      crew_true(workers, is.numeric(.), is.finite(.), length(.) == 1, . > 0)
      crew_true(inherits(store, "crew_store"))
      crew_true(timeout, is.numeric(.), !anyNA(.), length(.) == 1, . >= 0)
      crew_true(wait, is.numeric(.), !anyNA(.), length(.) == 1, . >= 0)
      crew_true(max_tasks, is.numeric(.), !anyNA(.), length(.) == 1, . >= 0)
      private$store <- store
      private$timeout <- timeout
      private$wait <- wait
      private$max_tasks <- max_tasks
      private$initialize_tasks()
      private$initialize_results()
      private$initialize_workers(workers = workers)
      invisible()
    },
    #' @description Show the queued tasks pushed with the `push()` method
    #'   but not yet assigned to workers.
    #' @return A data frame with one row per queued task
    #'   and columns describing each task.
    get_tasks = function() {
      private$tasks
    },
    #' @description Show the results completed by the workers and available
    #'   to retrieve with the `pop()` method.
    #' @return A data frame with one row per completed task
    #'   and columns describing each task.
    get_results = function() {
      private$results
    },
    #' @description Show the workers of the queue.
    #' @return A data frame with one row per worker
    #'   and columns describing each worker.
    get_workers = function() {
      private$workers
    },
    #' @description Push a new task on to the queue.
    #' @return `NULL` (invisibly)
    #' @param fun R function that runs the task.
    #' @param args Named list of arguments to `fun`.
    #' @param task Character of length 1 with the task ID.
    #' @param update Logical of length 1, whether to update the
    #'   internal state of the queue after pushing. See the
    #'   `update()` method for details.
    #' @param ... Named arguments specific to each job, such as the
    #'   `future` plan for [crew_queue_future] queues.
    push = function(
      fun,
      args = list(),
      task = crew_name(),
      update = TRUE,
      ...
    ) {
      fun <- rlang::as_function(fun)
      crew_true(is.function(fun))
      crew_true(is.list(args))
      crew_true(names(args), length(unique(.)) == length(args), nzchar(.))
      crew_true(update, is.logical(.), !anyNA(.), length(.) == 1)
      crew_true(task, is.character(.), !anyNA(.), length(.) == 1, nzchar(.))
      input <- list(fun = fun, args = args, ...)
      private$add_task(task = task, input = input)
      if (update) {
        private$update_all()
      }
      invisible()
    },
    #' @description Pop the results of a completed task off the queue.
    #' @return A named list with the results of the task.
    #' @param update Logical of length 1, whether to update the
    #'   internal state of the queue after pushing. See the
    #'   `update()` method for details.
    pop = function(update = TRUE) {
      crew_true(update, is.logical(.), !anyNA(.), length(.) == 1)
      if (update) {
        private$update_all()
      }
      results <- private$results
      out <- NULL
      if (nrow(results)) {
        out <- list(task = results$task[1], result = results$result[[1]])
        private$results <- private$results[-1, ]
      }
      out
    },
    #' @description Update the internal state of the queue.
    #' @details Updating moves tasks along and make sure they get
    #'   assigned to workers and cleaned up in a timely manner.
    #'   However, there may be a computational overhead cost
    #'   if updates happen too often. It is recommended to have
    #'   an event loop that updates the queue, (preferably via
    #'   methods `push()` and `pop()` instead of `update()` directly)
    #'   that updates the queue at a time interval that appropriately
    #'   balances the tradeoffs.
    #' @return `NULL` (invisibly)
    update = function() {
      private$update_all()
      invisible()
    },
    #' @description Detect crashed workers.
    #' @details This usually happens during `update()` anyway.
    #' @return Return `NULL` (invisibly) if no workers crashed.
    #'   Throw an error if at least one worker crashed.
    crashed = function() {
      private$update_crashed()
      invisible()
    },
    #' @description Block the R session until there are no unassigned
    #'   tasks and at least one worker is available.
    #' @details The `block()` method calls `update()` every `wait` seconds
    #'   until the backlog is clear or `timeout` is reached. It is a
    #'   convenient event loop if you want to clear the backlog before
    #'   taking any other action, but most event loops continuously
    #'   assign tasks with `push()` and retrieve available ones with `pop()`.
    #' @return `NULL` (invisibly)
    #' @param timeout Number of seconds to wait to clear the queue backlog.
    #'   Defaults to the `timeout` value assigned at initialization.
    #' @param wait Number of seconds to wait in between update iterations.
    #'   Defaults to the `wait` value assigned at initialization.
    block = function(timeout = NULL, wait = NULL) {
      crew_true(
        timeout %|||% 0,
        is.numeric(.),
        length(.) == 1,
        . >= 0,
        !anyNA(.)
      )
      crew_true(
        wait %|||% 0,
        is.numeric(.),
        length(.) == 1,
        . >= 0,
        !anyNA(.)
      )
      private$available_wait(timeout = timeout, wait = wait)
      invisible()
    },
    #' @description Shut down the workers.
    #' @details Not all queues can shut down all their workers.
    #'   For example, `shutdown()` in [crew_queue_future] can shut down its
    #'   `callr`-based subqueue, but it cannot shut down its `future`
    #'   workers. Local subqueues such as [crew_queue_session]
    #'   and [crew_queue_bg] can shut down completely.
    #' @return `NULL` (invisibly)
    shutdown = function() {
      if (!is.null(private$subqueue$shutdown)) {
        private$subqueue$shutdown()
      }
      invisible()
    }
  )
)

crew_queue_worker_start <- function(
  worker,
  store,
  max_tasks,
  timeout,
  wait,
  log
) {
  crew::crew_worker(
    worker = worker,
    store = store,
    max_tasks = max_tasks,
    timeout = timeout,
    wait = wait
  )
}
