#' @title Worker event loop.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @details The worker event loop runs inside the underlying process
#'   of a worker. It waits for the next job (timing out
#'   if it idles for too long at one time) runs any incoming jobs
#'   and puts the output in the data store.
#' @return `NULL` (invisibly).
#' @inheritParams crew_job
#' @param jobs Maximum number of jobs before returning.
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
#'   jobs = 1,
#'   timeout = 0,
#'   wait = 0
#' )
#' store$read_worker_output("my_worker")$result # 2
#' }
crew_worker <- function(worker, store, jobs, timeout, wait) {
  worker <- as.character(worker)
  store <- eval(parse(text = store))
  jobs <- as.numeric(jobs)
  timeout <- as.numeric(timeout)
  wait <- as.numeric(wait)
  job <- 0
  while (job < jobs) {
    crew_iterate(
      worker = worker,
      store = store,
      timeout = timeout,
      wait = wait
    )
    job <- job + 1
  }
  invisible()
}

crew_iterate <- function(worker, store, timeout, wait) {
  crew_wait(
    fun = ~.x$exists_worker_input(worker = .y),
    args = list(store = store, worker = worker),
    timeout = timeout,
    wait = wait
  )
  crew_job(worker = worker, store = store, timeout = timeout, wait = wait)
}

#' @title Run a job in a crew worker.
#' @description Not a user-side function. Do not invoke directly.
#' @export
#' @keywords internal
#' @param worker Character of length 1, name of the worker.
#' @param store Either a data store object or a character string
#'   with a marshaled data store.
#' @param timeout Positive numeric of length 1, number of seconds
#'   that a worker can idle before timing out.
#' @param wait Positive numeric of length 1, number of seconds
#'   that the worker waits between checking if a job exists.
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
#' crew_job(
#'   worker = "my_worker",
#'   store = store$marshal(),
#'   timeout = 0,
#'   wait = 0
#' )
#' store$read_worker_output("my_worker")$result # 2
#' }
crew_job <- function(worker, store, timeout, wait) {
  if (is.character(store)) {
    store <- eval(parse(text = store))
  }
  tryCatch({
      input <- store$read_worker_input(worker = worker)
      input$fun <- eval(parse(text = input$fun))
      value <- crew_monad(fun = input$fun, args = input$args)
      store$delete_worker_input(worker = worker)
      store$write_worker_output(worker = worker, value = value)
    },
    error = function(condition) {
      store$write_worker_error(worker = worker, value = condition)
      crew_error(conditionMessage(condition))
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
