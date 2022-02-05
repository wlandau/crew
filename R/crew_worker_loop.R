#' @title Local worker event loop.
#' @export
#' @description Local worker event loop.
#' @return `NULL` (invisibly).
#' @inheritParams crew_worker_loop_run
crew_worker_loop <- function(name, store, timeout, wait_input) {
  name <- as.character(name)
  store <- eval(parse(text = store))
  timeout <- as.numeric(timeout)
  wait_input <- as.numeric(wait_input)
  store$validate()
  tryCatch(
    crew::crew_worker_loop_run(
      name = name,
      store = store,
      timeout = timeout,
      wait_input = wait_input
    ),
    crew_shutdown = identity
  )
  invisible()
}

#' @title Local worker inner event loop.
#' @export
#' @keywords internal
#' @description Not a user-side function. for internal use only.
#' @return `NULL` (invisibly).
#' @param name Character of length 1, name of the worker.
#' @param store Marshaled `R6` data store object to
#'   send/receive job data from the crew.
#'   Constructed from the `$marshal()` method of the store.
#'   `eval(parse(text = store))`
#'   should reconstruct the usable store object inside the worker event loop.
#' @param timeout Positive numeric of length 1, number of seconds
#'   that a worker can idle before timing out.
#' @param wait_input Positive numeric of length 1, number of seconds
#'   that the worker waits between checking if a job exists.
crew_worker_loop_run <- function(name, store, timeout, wait_input) {
  start <- proc.time()["elapsed"]
  do_while <- FALSE
  while (do_while || (proc.time()["elapsed"] - start < timeout)) {
    if (store$exists_input(name = name)) {
      crew_worker_loop_job(name = name, store = store)
      start <- proc.time()["elapsed"]
    } else {
      Sys.sleep(wait_input) # nocov (coverage here is a race condition)
    }
    do_while <- TRUE
  }
  crew_timeout(sprintf("worker %s timed out at %s seconds.", name, timeout))
}

crew_worker_loop_job <- function(name, store) {
  input <- store$read_input(name = name)
  store$delete_input(name = name)
  input$fun <- eval(parse(text = input$fun))
  output <- do.call(what = input$fun, args = input$args)
  store$write_output(name = name, data = output)
}
