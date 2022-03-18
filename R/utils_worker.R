#' @title Worker event loop.
#' @export
#' @keywords internal
#' @description Not a user-side function. Do not invoke directly.
#' @details The worker event loop runs inside the underlying process
#'   of a worker. It waits for the next job (timing out
#'   if it idles for too long at one time) runs any incoming jobs
#'   and puts the output in the data store. If sent a shutdown job
#'   (which throws an error of class `"crew_shutdown"`) then the
#'   loop exits.
#' @return `NULL` (invisibly).
#' @param timeout Positive numeric of length 1, number of seconds
#'   that a worker can idle before timing out.
#' @param wait Positive numeric of length 1, number of seconds
#'   that the worker waits between checking if a job exists.
#' @examples
#' if (!identical(Sys.getenv("CREW_EXAMPLES", unset = ""), "")) {
#' dir_root <- tempfile()
#' dir.create(dir_root)
#' store <- store_local$new(dir_root = dir_root)
#' fun <- function(x) {
#'   x + 1
#' }
#' args <- list(x = 1)
#' value <- list(fun = deparse(fun), args = args)
#' store$write_worker_input(worker = "my_worker", value = value)
#' try( # The worker throws a special error class when it times out.
#'   crew_worker(
#'     worker = "my_worker",
#'     store = store$marshal(),
#'     timeout = 0,
#'     wait = 0
#'   ),
#'   silent = TRUE
#' )
#' store$read_worker_output("my_worker")$result # 2
#' }
crew_worker <- function(worker, store, timeout, wait) {
  worker <- as.character(worker)
  store <- eval(parse(text = store))
  timeout <- as.numeric(timeout)
  wait <- as.numeric(wait)
  tryCatch(
    while (TRUE) {
      crew_wait(
        fun = ~.x$exists_worker_input(worker = .y),
        args = list(store = store, worker = worker),
        timeout = timeout,
        wait = wait
      )
      crew_job(worker, store, timeout, wait)
    },
    crew_shutdown = identity
  )
  invisible()
}

crew_job <- function(worker, store, timeout, wait) {
  input <- store$read_worker_input(worker = worker)
  input$fun <- eval(parse(text = input$fun))
  value <- crew_monad(fun = input$fun, args = input$args)
  store$delete_worker_input(worker = worker)
  store$write_worker_output(worker = worker, value = value)
  if (isTRUE(value$shutdown)) {
    crew_shutdown()
  }
}

crew_monad <- function(fun, args) {
  capture_error <- function(condition) {
    state$shutdown <- inherits(condition, "crew_shutdown")
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
    shutdown = state$shutdown,
    traceback = state$traceback,
    warnings = state$warnings
  )
}
