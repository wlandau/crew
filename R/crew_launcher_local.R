#' @title Create a launcher with local process workers.
#' @export
#' @family plugin_local
#' @description Create an `R6` object to launch and maintain
#'   local process workers.
#' @inheritParams crew_launcher
#' @param local_log_directory Either `NULL` or a character of length 1
#'   with the file path to a directory to write worker-specific log files
#'   with standard output and standard error messages.
#'   Each log file represents a single *instance* of a running worker,
#'   so there will be more log files
#'   if a given worker starts and terminates a lot. Set to `NULL` to suppress
#'   log files (default).
#' @param local_log_join Logical of length 1. If `TRUE`, `crew` will write
#'   standard output and standard error to the same log file for
#'   each worker instance. If `FALSE`, then they these two streams
#'   will go to different log files with informative suffixes.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local(name = client$name)
#' launcher$start(sockets = client$summary()$socket)
#' launcher$launch(index = 1L)
#' task <- mirai::mirai("result", .compute = client$name)
#' mirai::call_mirai_(task)
#' task$data
#' client$terminate()
#' }
crew_launcher_local <- function(
  name = NULL,
  seconds_interval = 0.5,
  seconds_timeout = 60,
  seconds_launch = 30,
  seconds_idle = Inf,
  seconds_wall = Inf,
  seconds_exit = NULL,
  tasks_max = Inf,
  tasks_timers = 0L,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  launch_max = 5L,
  tls = crew::crew_tls(),
  local_log_directory = NULL,
  local_log_join = TRUE
) {
  crew_deprecate(
    name = "seconds_exit",
    date = "2023-09-21",
    version = "0.5.0.9002",
    alternative = "none (no longer necessary)",
    condition = "warning",
    value = seconds_exit
  )
  name <- as.character(name %|||% crew_random_name())
  launcher <- crew_class_launcher_local$new(
    name = name,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    seconds_launch = seconds_launch,
    seconds_idle = seconds_idle,
    seconds_wall = seconds_wall,
    tasks_max = tasks_max,
    tasks_timers = tasks_timers,
    reset_globals = reset_globals,
    reset_packages = reset_packages,
    reset_options = reset_options,
    garbage_collection = garbage_collection,
    launch_max = launch_max,
    tls = tls,
    local_log_directory = local_log_directory,
    local_log_join = local_log_join
  )
  launcher$validate()
  launcher
}

#' @title Local process launcher class
#' @export
#' @family plugin_local
#' @description `R6` class to launch and manage local process workers.
#' @details See [crew_launcher_local()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' launcher <- crew_launcher_local(name = client$name)
#' launcher$start(sockets = client$summary()$socket)
#' launcher$launch(index = 1L)
#' task <- mirai::mirai("result", .compute = client$name)
#' mirai::call_mirai_(task)
#' task$data
#' client$terminate()
#' }
crew_class_launcher_local <- R6::R6Class(
  classname = "crew_class_launcher_local",
  inherit = crew_class_launcher,
  cloneable = FALSE,
  private = list(
    .local_log_directory = NULL,
    .local_log_join = NULL,
    .log_prepare = function() {
      if (!is.null(private$.local_log_directory)) {
        dir_create(private$.local_log_directory)
      }
    },
    .log_stdout = function(name) {
      directory <- private$.local_log_directory
      if (is.null(directory)) {
        return(NULL)
      }
      if (!private$.local_log_join) {
        name <- paste0(name, "-stdout")
      }
      file.path(directory, paste0(name, ".log"))
    },
    .log_stderr = function(name) {
      directory <- private$.local_log_directory
      if (is.null(directory)) {
        return(NULL)
      }
      if_any(
        private$.local_log_join,
        "2>&1",
        file.path(directory, paste0(name, "-stderr.log"))
      )
    }
  ),
  active = list(
    #' @field local_log_directory See [crew_launcher_local()].
    local_log_directory = function() {
      .subset2(private, ".local_log_directory")
    },
    #' @field local_log_join See [crew_launcher_local()].
    local_log_join = function() {
      .subset2(private, ".local_log_join")
    }
  ),
  public = list(
    #' @description Local launcher constructor.
    #' @return An `R6` object with the local launcher.
    #' @param name See [crew_launcher()].
    #' @param seconds_interval See [crew_launcher()].
    #' @param seconds_timeout See [crew_launcher()].
    #' @param seconds_launch See [crew_launcher()].
    #' @param seconds_idle See [crew_launcher()].
    #' @param seconds_wall See [crew_launcher()].
    #' @param seconds_exit See [crew_launcher()].
    #' @param tasks_max See [crew_launcher()].
    #' @param tasks_timers See [crew_launcher()].
    #' @param reset_globals See [crew_launcher()].
    #' @param reset_packages See [crew_launcher()].
    #' @param reset_options See [crew_launcher()].
    #' @param garbage_collection See [crew_launcher()].
    #' @param launch_max See [crew_launcher()].
    #' @param tls See [crew_launcher()].
    #' @param processes See [crew_launcher()].
    #' @param local_log_directory See [crew_launcher_local()].
    #' @param local_log_join See [crew_launcher_local()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' client <- crew_client()
    #' client$start()
    #' launcher <- crew_launcher_local(name = client$name)
    #' launcher$start(sockets = client$summary()$socket)
    #' launcher$launch(index = 1L)
    #' task <- mirai::mirai("result", .compute = client$name)
    #' mirai::call_mirai_(task)
    #' task$data
    #' client$terminate()
    #' }
    initialize = function(
      name = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      seconds_launch = NULL,
      seconds_idle = NULL,
      seconds_wall = NULL,
      seconds_exit = NULL,
      tasks_max = NULL,
      tasks_timers = NULL,
      reset_globals = NULL,
      reset_packages = NULL,
      reset_options = NULL,
      garbage_collection = NULL,
      launch_max = NULL,
      tls = NULL,
      processes = NULL,
      local_log_directory = NULL,
      local_log_join = NULL
    ) {
      super$initialize(
        name = name,
        seconds_interval = seconds_interval,
        seconds_timeout = seconds_timeout,
        seconds_launch = seconds_launch,
        seconds_idle = seconds_idle,
        seconds_wall = seconds_wall,
        seconds_exit = seconds_exit,
        tasks_max = tasks_max,
        tasks_timers = tasks_timers,
        reset_globals = reset_globals,
        reset_packages = reset_packages,
        reset_options = reset_options,
        garbage_collection = garbage_collection,
        launch_max = launch_max,
        tls = tls,
        processes = processes
      )
      private$.local_log_directory <- local_log_directory
      private$.local_log_join <- local_log_join
    },
    #' @description Validate the local launcher.
    #' @return `NULL` (invisibly).
    validate = function() {
      super$validate()
      crew_assert(
        private$.local_log_directory %|||% "x",
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = "local_log_directory must be NULL or a valid directory path."
      )
      crew_assert(
        private$.local_log_join,
        isTRUE(.) || isFALSE(.),
        message = "local_log_join must be TRUE or FALSE."
      )
    },
    #' @description Launch a local process worker which will
    #'   dial into a socket.
    #' @details The `call` argument is R code that will run to
    #'   initiate the worker. Together, the `launcher`, `worker`,
    #'   and `instance` arguments are useful for
    #'   constructing informative job names.
    #' @return A handle object to allow the termination of the worker
    #'   later on.
    #' @param call Character of length 1 with a namespaced call to
    #'   [crew_worker()] which will run in the worker and accept tasks.
    #' @param name Character of length 1 with a long informative worker name
    #'   which contains the `launcher`, `worker`, and `instance` arguments
    #'   described below.
    #' @param launcher Character of length 1, name of the launcher.
    #' @param worker Positive integer of length 1, index of the worker.
    #'   This worker index remains the same even when the current instance
    #'   of the worker exits and a new instance launches.
    #'   It is always between 1 and the maximum number of concurrent workers.
    #' @param instance Character of length 1 to uniquely identify
    #'   the current instance of the worker a the index in the launcher.
    launch_worker = function(call, name, launcher, worker, instance) {
      bin <- if_any(tolower(Sys.info()[["sysname"]]) == "windows", "R.exe", "R")
      path <- file.path(R.home("bin"), bin)
      private$.log_prepare()
      processx::process$new(
        command = path,
        args = c("-e", call),
        cleanup = TRUE,
        stdout = private$.log_stdout(name = name),
        stderr = private$.log_stderr(name = name)
      )
    },
    #' @description Terminate a local process worker.
    #' @return A list with the process ID of the worker.
    #' @param handle A process handle object previously
    #'   returned by `launch_worker()`.
    terminate_worker = function(handle) {
      handle$signal(signal = crew_terminate_signal())
      list(pid = handle$get_pid())
    }
  )
)
