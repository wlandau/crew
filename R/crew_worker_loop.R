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
#' store <- class_store_local$new(dir_root = dir_root)
#' fun <- function(x) {
#'   x + 1
#' }
#' args <- list(x = 1)
#' data <- list(fun = deparse(fun), args = args)
#' store$write_input(name = "my_worker", data = data)
#' try( # The worker throws a special error class when it times out.
#'   crew_worker_loop(
#'     name = "my_worker",
#'     store = store$marshal(),
#'     timeout = 0,
#'     wait = 0
#'   ),
#'   silent = TRUE
#' )
#' store$read_output("my_worker")$value
crew_worker_loop <- function(name, store, timeout, wait) {
  name <- as.character(name)
  store <- eval(parse(text = store))
  timeout <- as.numeric(timeout)
  wait <- as.numeric(wait)
  store$validate()
  tryCatch(
    crew::crew_worker_loop_run(
      name = name,
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
#' @param name Character of length 1, name of the worker.
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
crew_worker_loop_run <- function(name, store, timeout, wait) {
  start <- proc.time()["elapsed"]
  do_while <- FALSE
  while (TRUE) {
    crew_wait(
      fun = function(name, store) store$exists_input(name = name),
      args = list(name = name, store = store),
      timeout = timeout,
      wait = wait
    )
    crew_worker_loop_job(name, store, timeout, wait)
  }
}

crew_worker_loop_job <- function(name, store, timeout, wait) {
  input <- store$read_input(name = name)
  input$fun <- eval(parse(text = input$fun))
  output <- crew_worker_loop_monad(fun = input$fun, args = input$args)
  class(output) <- class(input)
  store$write_output(
    name = name,
    data = output,
    timeout = timeout,
    wait = wait
  )
  store$delete_input(
    name = name,
    timeout = timeout,
    wait = wait
  )
  crew_worker_loop_job_finalize(input)
  invisible()
}

crew_worker_loop_job_finalize <- function(input) {
  UseMethod("crew_worker_loop_job_finalize")
}

#' @export
crew_worker_loop_job_finalize.default <- function(input) {
}

#' @export
crew_worker_loop_job_finalize.shutdown <- function(input) {
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
  value <- tryCatch(
    expr = withCallingHandlers(
      expr = do.call(what = fun, args = args),
      error = capture_error,
      warning = capture_warnings
    ),
    error = function(condition) NULL
  )
  seconds <- as.numeric(proc.time()["elapsed"]) - start
  list(
    value = value,
    seconds = seconds,
    error = state$error,
    traceback = state$traceback,
    warnings = state$warnings
  )
}
