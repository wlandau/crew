crew_store_local <- R6::R6Class(
  classname = "crew_store_local",
  inherit = crew_store,
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    send_file = function(from, to) {
      file.rename(from = from, to = to)
      crew_wait(
        fun = function(path) file.exists(to) && !file.exists(from),
        args = list(path = path),
        timeout = private$timeout,
        wait = private$wait
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
    write_worker_input = function(worker, value) {
      private$write_local(dir = "worker_input", worker = worker, value = value)
    },
    write_worker_output = function(worker, value) {
      private$write_local(dir = "worker_output", worker = worker, value = value)
    },
    exists_worker_input = function(worker) {
      private$exists_local(dir = "worker_input", worker = worker)
    },
    exists_worker_output = function(worker) {
      private$exists_local(dir = "worker_output", worker = worker)
    },
    delete_worker_input = function(worker) {
      private$delete_local(dir = "worker_input", worker = worker)
    },
    delete_worker_output = function(worker) {
      private$delete_local(dir = "worker_output", worker = worker)
    }
  )
)
