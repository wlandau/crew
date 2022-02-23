crew_store <- R6::R6Class(
  classname = "crew_store",
  portable = FALSE,
  cloneable = FALSE,
  private = list(
    dir_root = NULL,
    timeout = NULL,
    wait = NULL,
    read_local = function(dir, worker) {
      crew_assert_chr_scalar(dir)
      path <- file.path(private$dir_root, dir, worker)
      crew_assert_file_scalar(path)
      qs::qread(file = path)
    },
    write_local = function(dir, worker, value) {
      crew_assert_chr_scalar(dir)
      crew_assert_chr_scalar(worker)
      path_temp <- file.path(private$dir_root, "temp", basename(tempfile()))
      path <- file.path(private$dir_root, dir, worker)
      dir_create(dirname(path_temp))
      dir_create(dirname(path))
      qs::qsave(x = value, file = path_temp)
      crew_wait(
        fun = function(path_temp) file.exists(path_temp),
        args = list(path_temp = path_temp),
        timeout = private$timeout,
        wait = private$wait
      )
      file.rename(from = path_temp, to = path)
      crew_wait(
        fun = function(path, path_temp) {
          file.exists(path) && !file.exists(path_temp)
        },
        args = list(path = path, path_temp = path_temp),
        timeout = private$timeout,
        wait = private$wait
      )
      invisible()
    },
    exists_local = function(dir, worker) {
      crew_assert_chr_scalar(dir)
      crew_assert_chr_scalar(worker)
      all(file.exists(file.path(private$dir_root, dir, worker)))
    },
    list_local = function(dir) {
      crew_assert_chr_scalar(dir)
      list.files(file.path(private$dir_root, dir))
    },
    delete_local = function(dir, worker) {
      crew_assert_chr_scalar(dir)
      crew_assert_chr_scalar(worker)
      path <- file.path(private$dir_root, dir, worker)
      unlink(path, recursive = TRUE, force = TRUE)
      crew_wait(
        fun = function(path) !file.exists(path),
        args = list(path = path),
        timeout = private$timeout,
        wait = private$wait
      )
      invisible()
    }
  ),
  public = list(
    initialize = function(
      dir_root = tempfile(),
      timeout = 60,
      wait = 1
    ) {
      crew_assert_chr_scalar(dir_root)
      private$dir_root <- dir_root
      private$timeout <- timeout
      private$wait <- wait
    },
    get_root = function() {
      private$dir_root
    },
    read_main_input = function(worker) {
      private$read_local(dir = "main_input", worker = worker)
    },
    read_main_output = function(worker) {
      private$read_local(dir = "main_output", worker = worker)
    },
    write_main_input = function(worker, value) {
      private$write_local(dir = "main_input", worker = worker, value = value)
    },
    write_main_output = function(worker, value) {
      private$write_local(dir = "main_output", worker = worker, value = value)
    },
    exists_main_input = function(worker) {
      private$exists_local(dir = "main_input", worker = worker)
    },
    exists_main_output = function(worker) {
      private$exists_local(dir = "main_output", worker = worker)
    },
    list_main_input = function() {
      private$list_local(dir = "main_input")
    },
    list_main_output = function() {
      private$list_local(dir = "main_output")
    },
    delete_main_input = function(worker) {
      private$delete_local(dir = "main_input", worker = worker)
    },
    delete_main_output = function(worker) {
      private$delete_local(dir = "main_output", worker = worker)
    },
    upload_input = function(worker) {
      from <- file.path(private$dir_root, "main_input", worker)
      to <- file.path(private$dir_root, "worker_input", worker)
      private$send_file(from = from, to = to)
    },
    download_output = function(worker) {
      from <- file.path(private$dir_root, "worker_output", worker)
      to <- file.path(private$dir_root, "main_output", worker)
      private$send_file(from = from, to = to)
    },
    destroy = function() {
      unlink(private$dir_root, recursive = TRUE, force = TRUE)
    }
  )
)
