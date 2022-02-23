#' @title Worker event loop.
#' @export
#' @description Run the worker event loop.
#' @details The worker event loop runs inside the underlying process
#'   of a worker. It waits for the next job (timing out
#'   if it idles for too long at one time) runs any incoming jobs
#'   and puts the output in the data store. If sent a shutdown job
#'   (which throws an error of class `"crew_shutdown"`) then the
#'   loop exits.
#' @return `NULL` (invisibly).
#' @inheritParams crew_worker_loop_run
#' @examples
#' dir_root <- tempfile()
#' dir.create(dir_root)
#' store <- crew_store_local$new(dir_root = dir_root)
#' fun <- function(x) {
#'   x + 1
#' }
#' args <- list(x = 1)
#' value <- list(fun = deparse(fun), args = args, class = "crew_task")
#' store$write_worker_input(worker = "my_worker", value = value)
#' try( # The worker throws a special error class when it times out.
#'   crew_worker_loop(
#'     worker = "my_worker",
#'     store = store$marshal(),
#'     timeout = 0,
#'     wait = 0
#'   ),
#'   silent = TRUE
#' )
#' store$read_worker_output("my_worker")$value
crew_worker_loop <- function(worker, store, timeout, wait) {
  worker <- as.character(worker)
  store <- eval(parse(text = store))
  timeout <- as.numeric(timeout)
  wait <- as.numeric(wait)
  tryCatch(
    crew::crew_worker_loop_run(
      worker = worker,
      store = store,
      timeout = timeout,
      wait = wait
    ),
    crew_shutdown = identity
  )
  invisible()
}

#' @title Local worker inner event loop.
#' @export
#' @keywords internal
#' @description Not a user-side function. for internal use only.
#' @details See [crew_worker_loop()] for details. `crew_worker_loop_run()`
#'   is the function that actually runs inside the loop and may
#'   error out with class `"crew_shutdown"`
#' @return `NULL` (invisibly).
#' @param worker Character of length 1, name of the worker.
#' @param store Marshaled `R6` data store object to
#'   send/receive job data from the crew.
#'   Constructed from the `$marshal()` method of the store.
#'   `eval(parse(text = store))`
#'   should reconstruct the usable store object inside the worker event loop.
#' @param timeout Positive numeric of length 1, number of seconds
#'   that a worker can idle before timing out.
#' @param wait Positive numeric of length 1, number of seconds
#'   that the worker waits between checking if a job exists.
#' @examples
#' # See the examples of crew_worker_loop().
crew_worker_loop_run <- function(worker, store, timeout, wait) {
  start <- proc.time()["elapsed"]
  do_while <- FALSE
  while (TRUE) {
    crew_wait(
      fun = function(worker, store) store$exists_worker_input(worker = worker),
      args = list(worker = worker, store = store),
      timeout = timeout,
      wait = wait
    )
    crew_worker_loop_job(worker, store, timeout, wait)
  }
}

crew_worker_loop_job <- function(worker, store, timeout, wait) {
  input <- store$read_worker_input(worker = worker)
  input$fun <- eval(parse(text = input$fun))
  value <- crew_worker_loop_monad(fun = input$fun, args = input$args)
  class(value) <- class(input)
  store$write_worker_output(worker = worker, value = value)
  store$delete_worker_input(worker = worker)
  crew_worker_loop_job_finalize(input)
  invisible()
}

crew_worker_loop_job_finalize <- function(input) {
  UseMethod("crew_worker_loop_job_finalize")
}

#' @export
crew_worker_loop_job_finalize.crew_task <- function(input) {
}

#' @export
crew_worker_loop_job_finalize.crew_shutdown <- function(input) {
  crew_shutdown()
}

crew_worker_loop_monad <- function(fun, args) {
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
