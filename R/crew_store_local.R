crew_store_local <- R6::R6Class(
  classname = "crew_store_local",
  inherit = crew_store,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    send_file = function(from, to) {
      crew_assert_file_scalar(from)
      crew_assert_chr_scalar(to)
      dir_create(dirname(to))
      file.rename(from = from, to = to)
      crew_wait(
        fun = function(from, to) file.exists(to) && !file.exists(from),
        args = list(from = from, to = to),
        timeout = private$timeout,
        wait = private$wait,
        "local store timeout sending file"
      )
      invisible()
    }
  ),
  public = list(
    read_worker_input = function(worker) {
      private$read_local(dir = "worker_input", worker = worker)
    },
    read_worker_output = function(worker) {
      private$read_local(dir = "worker_output", worker = worker)
    },
    read_worker_error = function(worker) {
      private$read_local(dir = "worker_error", worker = worker)
    },
    write_worker_input = function(worker, value) {
      private$write_local(dir = "worker_input", worker = worker, value = value)
    },
    write_worker_output = function(worker, value) {
      private$write_local(dir = "worker_output", worker = worker, value = value)
    },
    write_worker_error = function(worker, value) {
      private$write_local(dir = "worker_error", worker = worker, value = value)
    },
    exists_worker_input = function(worker) {
      private$exists_local(dir = "worker_input", worker = worker)
    },
    exists_worker_output = function(worker) {
      private$exists_local(dir = "worker_output", worker = worker)
    },
    exists_worker_error = function(worker) {
      private$exists_local(dir = "worker_error", worker = worker)
    },
    list_worker_input = function() {
      private$list_local(dir = "worker_input")
    },
    list_worker_output = function() {
      private$list_local(dir = "worker_output")
    },
    list_worker_error = function() {
      private$list_local(dir = "worker_error")
    },
    delete_worker_input = function(worker) {
      private$delete_local(dir = "worker_input", worker = worker)
    },
    delete_worker_output = function(worker) {
      private$delete_local(dir = "worker_output", worker = worker)
    },
    delete_worker_error = function(worker) {
      private$delete_local(dir = "worker_error", worker = worker)
    },
    marshal = function() {
      expr <- substitute(
        crew:::crew_store_local$new(
          dir_root = dir_root,
          timeout = timeout,
          wait = wait
        ),
        list(
          dir_root = private$dir_root,
          timeout = private$timeout,
          wait = private$wait
        )
      )
      paste(deparse(expr), collapse = "\n")
    }
  )
)
