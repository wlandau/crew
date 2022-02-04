#' @title Local worker event loop.
#' @export
#' @description Local worker event loop.
#' @return `NULL` (invisibly).
#' @param name Character of length 1, name of the worker.
#' @param dir_root Character of length 1, directory path to the data store.
#' @param timeout Positive numeric of length 1, number of seconds
#'   that a worker can idle before timing out.
#' @param wait_input Positive numeric of length 1, number of seconds
#'   that the worker waits between checking if a job exists.
crew_loop_worker_local <- function(name, dir_root, timeout, wait_input) {
  tryCatch(
    crew_loop_worker_local_run(
      name = name,
      dir_root = dir_root,
      timeout = timeout,
      wait_input = wait_input
    ),
    crew_shutdown = identity
  )
  invisible()
}

crew_loop_worker_local_run <- function(name, dir_root, timeout, wait_input) {
  store <- class_store_local$new(dir_root = dir_root)
  start <- proc.time()["elapsed"]
  do_while <- FALSE
  while (do_while || (proc.time()["elapsed"] - start < timeout)) {
    if (store$exists_input(name = name)) {
      crew_loop_worker_local_job(name = name, store = store)
      start <- proc.time()["elapsed"]
    } else {
      Sys.sleep(wait_input) # nocov (coverage here is a race condition)
    }
    do_while <- TRUE
  }
  crew_timeout(sprintf("worker %s timed out at %s seconds.", name, timeout))
}

crew_loop_worker_local_job <- function(name, store) {
  input <- store$read_input(name = name)
  store$delete_input(name = name)
  input$fun <- eval(parse(text = input$fun))
  output <- do.call(what = input$fun, args = input$args)
  store$write_output(name = name, data = output)
}
