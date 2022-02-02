#' @export
crew_worker_local <- function(name, dir_root, timeout, wait_input) {
  tryCatch(
    crew_worker_local_run(
      name = name,
      dir_root = dir_root,
      timeout = timeout,
      wait_input = wait_input
    ),
    crew_terminate = identity
  )
}

crew_worker_local_run <- function(name, dir_root, timeout, wait_input) {
  store <- class_store_local$new(dir_root = dir_root)
  start <- proc.time()["elapsed"]
  while (proc.time()["elapsed"] - start < timeout) {
    if (store$exists_input(name = name)) {
      crew_worker_local_job(name = name, store = store)
      start <- proc.time()["elapsed"]
    } else {
      Sys.sleep(wait_input)
    }
  }
  crew_timeout(sprintf("worker %s timed out at %s seconds.", name, timeout))
}

crew_worker_local_job <- function(name, store) {
  input <- store$read_input(name = name)
  input$fun <- deparse(fun)
  output <- do.call(what = input$fun, args = input$args)
  store$write_output(name = name, data = output)
}
