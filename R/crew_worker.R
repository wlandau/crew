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
#' @examples
#' if (!identical(Sys.getenv("CREW_EXAMPLES", unset = ""), "")) {
#' root <- tempfile()
#' dir.create(root)
#' store <- crew_store_local$new(root = root)
#' fun <- function(x) {
#'   x + 1
#' }
#' args <- list(x = 1)
#' value <- list(fun = deparse(fun), args = args)
#' store$write_input(worker = "my_worker", value = value)
#' log <- tempfile()
#' crew_worker(
#'   worker = "my_worker",
#'   store = store$marshal(),
#'   max_tasks = 1,
#'   timeout = 0,
#'   wait = 0,
#'   log = log
#' )
#' store$read_output("my_worker")$result # 2
#' writeLines(readLines(log))
#' }
crew_worker <- function(
  worker,
  store,
  max_tasks,
  timeout,
  wait,
  log = NULL
) {
  if (!is.null(log)) {
    crew_true(log, is.character(.), nzchar(.), !anyNA(.), length(.) == 1)
    unlink(log)
  }
  worker <- as.character(worker)
  store <- eval(parse(text = store))
  max_tasks <- as.numeric(max_tasks)
  timeout <- as.numeric(timeout)
  wait <- as.numeric(wait)
  task <- 0
  while (task < max_tasks) {
    crew_iterate(
      worker = worker,
      store = store,
      timeout = timeout,
      wait = wait,
      log = log
    )
    task <- task + 1
  }
  invisible()
}

crew_iterate <- function(worker, store, timeout, wait, log) {
  crew_wait(
    fun = ~.x$exists_input(worker = .y),
    args = list(store = store, worker = worker),
    timeout = timeout,
    wait = wait
  )
  crew_task(
    worker = worker,
    store = store,
    timeout = timeout,
    wait = wait,
    log = log
  )
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
#' root <- tempfile()
#' dir.create(root)
#' store <- crew_store_local$new(root = root)
#' fun <- function(x) {
#'   x + 1
#' }
#' args <- list(x = 1)
#' value <- list(fun = deparse(fun), args = args)
#' store$write_input(worker = "my_worker", value = value)
#' log <- tempfile()
#' crew_task(
#'   worker = "my_worker",
#'   store = store$marshal(),
#'   timeout = 0,
#'   wait = 0,
#'   log = log
#' )
#' store$read_output("my_worker")$result # 2
#' writeLines(readLines(log))
#' }
crew_task <- function(worker, store, timeout, wait, log = NULL) {
  if (is.character(store)) {
    store <- eval(parse(text = store))
  }

  tryCatch({
    message("reading worker ", worker, " input")
    write("reading", "~/Desktop/log.txt", append = TRUE)
      input <- store$read_input(worker = worker)
      
      message("parsing worker ", worker, " function")
      write("parsing", "~/Desktop/log.txt", append = TRUE)
      input$fun <- eval(parse(text = input$fun))
      
      message("running worker ", worker, " task")
      write("running", "~/Desktop/log.txt", append = TRUE)
      value <- crew_monad(fun = input$fun, args = input$args)
      
      message("deleting worker ", worker, " input")
      write("deleting", "~/Desktop/log.txt", append = TRUE)
      store$delete_input(worker = worker)
      
      message("writing worker ", worker, " output")
      write("writing", "~/Desktop/log.txt", append = TRUE)
      #store$write_output(worker = worker, value = value)
      
      store$private$write_task(direction = "output", worker = worker, value = value)
      
      message("wrote worker ", worker, " output")
      write("done", "~/Desktop/log.txt", append = TRUE)
    },
    error = function(condition) {
      message <- paste(worker, "worker error:", conditionMessage(condition))
      crew::crew_error(message)
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
