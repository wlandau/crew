#' @title Worker event loop.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @details The worker event loop runs inside the underlying process
#'   of a worker. It waits for the next task (timing out
#'   if it idles for too long at one time) runs any incoming tasks
#'   and puts the output in the data store.
#' @return `NULL` (invisibly).
#' @inheritParams crew_task
#' @param max_tasks Maximum number of tasks before returning.
#' @param log Character of length 1, path to a log file to write.
#' @examples
#' if (!identical(Sys.getenv("CREW_EXAMPLES", unset = ""), "")) {
#' dir_root <- tempfile()
#' dir.create(dir_root)
#' store <- crew_store_local$new(dir_root = dir_root)
#' fun <- function(x) {
#'   x + 1
#' }
#' args <- list(x = 1)
#' value <- list(fun = deparse(fun), args = args)
#' store$write_worker_input(worker = "my_worker", value = value)
#' crew_worker(
#'   worker = "my_worker",
#'   store = store$marshal(),
#'   max_tasks = 1,
#'   timeout = 0,
#'   wait = 0
#' )
#' store$read_worker_output("my_worker")$result # 2
#' }
crew_worker <- function(
    worker,
    store,
    max_tasks,
    timeout,
    wait,
    log = tempfile()
  ) {
  crew_true(log, is.character(.), !anyNA(.), nzchar(.), length(.) == 1)
  connection <- file(log, open = "wt")
  sink(connection, type = "message")
  on.exit(sink(NULL, type = "message"))
  on.exit(close(connection), add = TRUE)
  worker <- as.character(worker)
  store <- eval(parse(text = store))
  max_tasks <- as.numeric(max_tasks)
  timeout <- as.numeric(timeout)
  wait <- as.numeric(wait)
  task <- 0
  while (task < max_tasks) {
    # crew_log(worker, "task", task, "of", max_tasks)
    crew_iterate(
      worker = worker,
      store = store,
      timeout = timeout,
      wait = wait
    )
    task <- task + 1
  }
  invisible()
}

crew_iterate <- function(worker, store, timeout, wait) {
  # crew_log(worker, "waiting for input")
  crew_wait(
    fun = ~.x$exists_worker_input(worker = .y),
    args = list(store = store, worker = worker),
    timeout = timeout,
    wait = wait
  )
  # crew_log(worker, "found task")
  crew_task(worker = worker, store = store, timeout = timeout, wait = wait)
}

#' @title Run a task in a crew worker.
#' @description Not a user-side function. Do not invoke directly.
#' @export
#' @keywords internal
#' @param worker Character of length 1, name of the worker.
#' @param store Either a data store object or a character string
#'   with a marshaled data store.
#' @param timeout Positive numeric of length 1, number of seconds
#'   that a worker can idle before timing out.
#' @param wait Positive numeric of length 1, number of seconds
#'   that the worker waits between checking if a task exists.
#' @examples
#' if (!identical(Sys.getenv("CREW_EXAMPLES", unset = ""), "")) {
#' dir_root <- tempfile()
#' dir.create(dir_root)
#' store <- crew_store_local$new(dir_root = dir_root)
#' fun <- function(x) {
#'   x + 1
#' }
#' args <- list(x = 1)
#' value <- list(fun = deparse(fun), args = args)
#' store$write_worker_input(worker = "my_worker", value = value)
#' crew_task(
#'   worker = "my_worker",
#'   store = store$marshal(),
#'   timeout = 0,
#'   wait = 0
#' )
#' store$read_worker_output("my_worker")$result # 2
#' }
crew_task <- function(worker, store, timeout, wait) {
  if (is.character(store)) {
    store <- eval(parse(text = store))
  }
  # crew_log("worker", worker, "found store")
  tryCatch({
      # crew_log(worker, "reading input")
      input <- store$read_worker_input(worker = worker)
      # crew_log(worker, "parsing function")
      input$fun <- eval(parse(text = input$fun))
      # crew_log(worker, "running task", input$task)
      value <- crew_monad(fun = input$fun, args = input$args)
      # crew_log(worker, "deleting old input")
      store$delete_worker_input(worker = worker)
      
      writeLines("x", "~/Desktop/worker.txt", worker)
      
      # crew_log(worker, "writing worker output")
      
       store$write_worker_output(worker = worker, value = value)
       
       writeLines("y", "~/Desktop/worker.txt")
       
       
      # crew_log(worker, "done writing task", input$task)
    },
    error = function(condition) {
      # crew_log(worker, "worker error:", conditionMessage(condition))
      crew::crew_error(conditionMessage(condition))
    }
  )
  invisible()
}

crew_monad <- function(fun, args) {
  capture_error <- function(condition) {
    state$error <- conditionMessage(condition)
    state$traceback <- as.character(sys.calls())
    NULL
  }
  capture_warnings <- function(condition) {
    state$warnings <- c(state$warnings, conditionMessage(condition))
  }
  state <- new.env(hash = FALSE, parent = emptyenv())
  start <- as.numeric(proc.time()["elapsed"])
  result <- tryCatch(
    expr = withCallingHandlers(
      expr = do.call(what = fun, args = args),
      error = capture_error,
      warning = capture_warnings
    ),
    error = function(condition) NULL
  )
  seconds <- as.numeric(proc.time()["elapsed"]) - start
  list(
    result = result,
    seconds = seconds,
    error = state$error,
    traceback = state$traceback,
    warnings = state$warnings
  )
}

crew_name <- function(n = 1) {
  out <- uuid::UUIDgenerate(n = n)
  out <- gsub("-", "_", out)
  paste0("x", out)
}

#' @title Print a [crew_worker()] log message.
#' @description Not a user-side function. Do not invoke directly.
#' @export
#' @keywords internal
#' @return Nothing (invisibly) but print a message.
#' @param ... Character strings to print.
#' @examples
#' crew_log("done")
crew_log <- function(...) {
  message("x")
 # time <- format(Sys.time(), "%z UTC %Y-%m-%d | %H:%M %OS2 |")
#  crew_message(paste(time, ...))
}
