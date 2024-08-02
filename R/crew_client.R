#' @title Create a client object.
#' @export
#' @family client
#' @description Create an `R6` wrapper object to manage the `mirai` client.
#' @param name Name of the client object. If `NULL`, a name is automatically
#'   generated.
#' @param workers Integer, maximum number of parallel workers to run.
#' @param host IP address of the `mirai` client to send and receive tasks.
#'   If `NULL`, the host defaults to the local IP address.
#' @param port TCP port to listen for the workers. If `NULL`,
#'   then an available ephemeral port is automatically chosen.
#' @param tls A TLS configuration object from [crew_tls()].
#' @param tls_enable Deprecated on 2023-09-15 in version 0.4.1.
#'   Use argument `tls` instead.
#' @param tls_config Deprecated on 2023-09-15 in version 0.4.1.
#'   Use argument `tls` instead.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete,
#'   such as checking `mirai::status()`
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete,
#'   such as checking `mirai::status()`.
#' @param retry_tasks `TRUE` to automatically retry a task in the event of
#'   an unexpected worker exit. `FALSE` to give up on the first exit and
#'   return a `mirai` error code (code number 19).
#'   `TRUE` (default) is recommended in most situations.
#'   Use `FALSE` for debugging purposes, e.g. to confirm that a task
#'   is causing a worker to run out of memory or crash in some other way.
#' @param log_resources Optional character string with a file path to a
#'   text file to log memory consumption.
#'   Set `log_resources` to `NULL` to avoid writing to a log file.
#'   If you supply a path, then
#'   the `log()` method will write memory usage statistics to the file,
#'   and most controller methods will do the same with throttling
#'   so resource consumption is recorded throughout the whole lifecycle
#'   of the controller.
#'
#'   The log file is in comma-separated values
#'   (CSV) format which can be easily read by `readr::read_csv()`.
#'   The controller automatically deletes the old log file when it starts
#'   (when `controller$start()` is called for the first time, but not
#'   subsequent times).
#'
#'   The log file has one row per observation of a process,
#'   including the current
#'   R process ("client") and the `mirai` dispatcher. If the dispatcher
#'   is not included in the output, it means the dispatcher process
#'   is not running.
#'   Columns include:
#'     * `type`: the type of process (client or dispatcher)
#'     * `pid`: the process ID.
#'     * `status`: The process status (from `ps::ps_status()`).
#'     * `rss`: resident set size (RSS). RS is the total memory held by
#'       a process, including shared libraries which may also be
#'       in use by other processes. RSS is obtained
#'       from `ps::ps_memory_info()` and shown in bytes.
#'     * `elapsed`: number of elapsed seconds since the R process was
#'       started (from `proc.time()["elapsed"]`).
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' client$summary()
#' client$terminate()
#' }
crew_client <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  tls = crew::crew_tls(),
  tls_enable = NULL,
  tls_config = NULL,
  seconds_interval = 0.5,
  seconds_timeout = 5,
  retry_tasks = TRUE,
  log_resources = NULL
) {
  crew_deprecate(
    name = "tls_enable",
    date = "2023-09-15",
    version = "0.4.1",
    alternative = "argument tls and function crew_tls()",
    value = tls_enable
  )
  crew_deprecate(
    name = "tls_config",
    date = "2023-09-15",
    version = "0.4.1",
    alternative = "argument tls and function crew_tls()",
    value = tls_config
  )
  name <- as.character(name %|||% crew_random_name())
  workers <- as.integer(workers)
  host <- as.character(host %|||% getip::getip(type = "local"))
  port <- as.integer(port %|||% 0L)
  crew_assert(
    inherits(tls, "crew_class_tls"),
    message = "argument tls must be an object created by crew_tls()"
  )
  client <- crew_class_client$new(
    name = name,
    workers = workers,
    host = host,
    port = port,
    tls = tls,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    retry_tasks = retry_tasks,
    log_resources = log_resources,
    relay = crew_relay()
  )
  client$validate()
  client
}

#' @title `R6` client class.
#' @export
#' @family client
#' @description `R6` class for `mirai` clients.
#' @details See [crew_client()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' client$log()
#' client$terminate()
#' }
crew_class_client <- R6::R6Class(
  classname = "crew_class_client",
  cloneable = FALSE,
  private = list(
    .name = NULL,
    .workers = NULL,
    .host = NULL,
    .port = NULL,
    .tls = NULL,
    .seconds_interval = NULL,
    .seconds_timeout = NULL,
    .retry_tasks = NULL,
    .log_resources = NULL,
    .throttle = NULL,
    .relay = NULL,
    .started = NULL,
    .client = NULL,
    .dispatcher = NULL
  ),
  active = list(
    #' @field name See [crew_client()].
    name = function() {
      .subset2(private, ".name")
    },
    #' @field workers See [crew_client()].
    workers = function() {
      .subset2(private, ".workers")
    },
    #' @field host See [crew_client()].
    host = function() {
      .subset2(private, ".host")
    },
    #' @field port See [crew_client()].
    port = function() {
      .subset2(private, ".port")
    },
    #' @field tls See [crew_client()].
    tls = function() {
      .subset2(private, ".tls")
    },
    #' @field seconds_interval See [crew_client()].
    seconds_interval = function() {
      .subset2(private, ".seconds_interval")
    },
    #' @field seconds_timeout See [crew_client()].
    seconds_timeout = function() {
      .subset2(private, ".seconds_timeout")
    },
    #' @field retry_tasks See [crew_client()]
    retry_tasks = function() {
      .subset2(private, ".retry_tasks")
    },
    #' @field log_resources Path to the log file for logging resources.
    log_resources = function() {
      .subset2(private, ".log_resources")
    },
    #' @field throttle Throttle object for logging resources.
    throttle = function() {
      .subset2(private, ".throttle")
    },
    #' @field relay Relay object for event-driven programming on a downstream
    #'   condition variable.
    relay = function() {
      .subset2(private, ".relay")
    },
    #' @field started Whether the client is started.
    started = function() {
      .subset2(private, ".started")
    },
    #' @field client Process ID of the local process running the client.
    client = function() {
      .subset2(private, ".client")
    },
    #' @field dispatcher Process ID of the `mirai` dispatcher
    dispatcher = function() {
      .subset2(private, ".dispatcher")
    }
  ),
  public = list(
    #' @description `mirai` client constructor.
    #' @return An `R6` object with the client.
    #' @param name Argument passed from [crew_client()].
    #' @param workers Argument passed from [crew_client()].
    #' @param host Argument passed from [crew_client()].
    #' @param port Argument passed from [crew_client()].
    #' @param tls Argument passed from [crew_client()].
    #' @param seconds_interval Argument passed from [crew_client()].
    #' @param seconds_timeout Argument passed from [crew_client()].
    #' @param retry_tasks Argument passed from [crew_client()].
    #' @param log_resources Argument passed from [crew_client()].
    #' @param relay Argument passed from [crew_client()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' client <- crew_client()
    #' client$start()
    #' client$log()
    #' client$terminate()
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      host = NULL,
      port = NULL,
      tls = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      retry_tasks = NULL,
      log_resources = NULL,
      relay = NULL
    ) {
      private$.name <- name
      private$.workers <- workers
      private$.host <- host
      private$.port <- port
      private$.tls <- tls
      private$.seconds_interval <- seconds_interval
      private$.seconds_timeout <- seconds_timeout
      private$.retry_tasks <- retry_tasks
      private$.log_resources <- log_resources
      private$.relay <- relay
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        private$.name,
        is.character(.),
        length(.) == 1L,
        nzchar(.),
        !anyNA(.)
      )
      crew_assert(
        private$.workers,
        is.integer(.),
        length(.) == 1L,
        !anyNA(.), . > 0L
      )
      crew_assert(
        private$.host,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.)
      )
      crew_assert(private$.port, is.integer(.), length(.) == 1L, !anyNA(.))
      crew_assert(private$.port, . >= 0L, . <= 65535L)
      crew_assert(
        inherits(private$.tls, "crew_class_tls"),
        message = "argument tls must be an object created by crew_tls()"
      )
      fields <- c(
        ".seconds_interval",
        ".seconds_timeout"
      )
      for (field in fields) {
        crew_assert(
          private[[field]],
          is.numeric(.),
          length(.) == 1L,
          !is.na(.),
          . >= 0
        )
      }
      crew_assert(
        private$.retry_tasks,
        isTRUE(.) || isFALSE(.),
        message = "retry_tasks must be TRUE or FALSE"
      )
      if_any(
        is.null(private$.log_resources),
        NULL,
        crew_assert(
          private$.log_resources,
          is.character(.),
          length(.) == 1L,
          nzchar(.),
          !anyNA(.)
        )
      )
      if (!is.null(private$.throttle)) {
        crew_assert(
          inherits(private$.throttle, "crew_class_throttle"),
          message = "field 'throttle' must be an object from crew_throttle()"
        )
        private$.throttle$validate()
      }
      if_any(
        is.null(private$.client),
        NULL,
        crew_assert(inherits(private$.client, "ps_handle"))
      )
      if_any(
        is.null(private$.dispatcher),
        NULL,
        crew_assert(inherits(private$.dispatcher, "ps_handle"))
      )
      crew_assert(private$.seconds_timeout >= private$.seconds_interval)
      crew_assert(inherits(private$.relay, "crew_class_relay"))
      private$.relay$validate()
      invisible()
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    start = function() {
      if (isTRUE(private$.started)) {
        return(invisible())
      }
      url <- sprintf(
        "%s://%s:%s",
        if_any(private$.tls$mode == "none", "ws", "wss"),
        private$.host,
        private$.port
      )
      mirai::daemons(
        n = private$.workers,
        url = url,
        dispatcher = TRUE,
        seed = NULL,
        tls = private$.tls$client(),
        pass = private$.tls$password,
        token = TRUE,
        retry = private$.retry_tasks,
        .compute = private$.name
      )
      private$.client <- ps::ps_handle()
      # TODO: remove code that gets the dispatcher PID if the dispatcher
      # process becomes a C thread.
      # Begin dispatcher code.
      pid <- mirai::nextget("pid", .compute = private$.name)
      if (!is.null(pid)) {
        private$.dispatcher <- ps::ps_handle(pid = pid)
      }
      # End dispatcher code.
      private$.relay$set_from(self$condition())
      private$.relay$start()
      private$.started <- TRUE
      invisible()
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (!isTRUE(private$.started)) {
        return(invisible())
      }
      mirai::daemons(n = 0L, .compute = private$.name)
      private$.relay$terminate()
      private$.started <- FALSE
      # TODO: if the dispatcher process becomes a C thread,
      # delete these superfluous checks on the dispatcher.
      # Begin dispatcher checks.
      if (is.null(private$.dispatcher)) {
        return(invisible())
      }
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = private$.dispatcher),
          seconds_interval = private$.seconds_interval,
          seconds_timeout = private$.seconds_timeout
        ),
        error = function(condition) NULL
      )
      if_any(
        ps::ps_is_running(p = private$.dispatcher),
        try(
          crew_terminate_process(p = ps::ps_pid(private$.dispatcher)),
          silent = TRUE
        ),
        NULL
      )
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = ps::ps_pid(private$.dispatcher)),
          seconds_interval = private$.seconds_interval,
          seconds_timeout = private$.seconds_timeout
        ),
        error = function(condition) NULL
      )
      # End dispatcher checks.
      invisible()
    },
    #' @description Get the `nanonext` condition variable which tasks signal
    #'   on resolution.
    #' @return The `nanonext` condition variable which tasks signal
    #'   on resolution. The return value is `NULL` if the client
    #'   is not running.
    condition = function() {
      mirai::nextget(x = "cv", .compute = .subset2(self, "name"))
    },
    #' @description Get the true value of the `nanonext` condition variable.
    #' @details Subtracts a safety offset which was padded on start.
    #' @return The value of the `nanonext` condition variable.
    resolved = function() {
      condition <- .subset2(self, "condition")()
      if_any(
        is.null(condition),
        0L,
        nanonext::cv_value(condition)
      )
    },
    #' @description Show an informative worker log.
    #' @return A `tibble` with information on the workers, or `NULL`
    #'   if the client is not started. The `tibble` has 1 row
    #'   per worker and the following columns:
    #'   * `worker`: integer index of the worker.
    #'   * `online`: `TRUE` if the worker is online and connected to the
    #'     websocket URL, `FALSE` otherwise.
    #'   * `instances`: integer, number of instances of `mirai` daemons
    #'     (`crew` workers) that have connected to the websocket URL
    #'     during the life cycle of the listener.
    #'   * `assigned`: number of tasks assigned to the current websocket URL.
    #'   * `complete`: number of tasks completed at the current websocket URL.
    #'   * `socket`: websocket URL. `crew` changes the token at the end of the
    #'     URL path periodically as a safeguard while managing workers.
    summary = function() {
      if (!isTRUE(private$.started)) {
        return(NULL)
      }
      daemons <- daemons_info(
        name = private$.name,
        seconds_interval = private$.seconds_interval,
        seconds_timeout = private$.seconds_timeout
      )
      tibble::tibble(
        worker = seq_len(nrow(daemons)),
        online = as.logical(daemons[, "online"] > 0L),
        instances = as.integer(abs(daemons[, "instance"])),
        assigned = as.integer(daemons[, "assigned"]),
        complete = as.integer(daemons[, "complete"]),
        socket = as.character(rownames(daemons))
      )
    },
    #' @description Get resource usage of local `crew` processes.
    #' @return A `tibble` with one row per process, including the current
    #'   R process ("client") and the `mirai` dispatcher. If the dispatcher
    #'   is not included in the output, it means the dispatcher process
    #'   is not running.
    #'   Columns include:
    #'     * `type`: the type of process (client or dispatcher)
    #'     * `pid`: the process ID.
    #'     * `status`: The process status (from `ps::ps_status()`).
    #'     * `rss`: resident set size (RSS). RS is the total memory held by
    #'       a process, including shared libraries which may also be
    #'       in use by other processes. RSS is obtained
    #'       from `ps::ps_memory_info()` and shown in bytes.
    #'     * `elapsed`: number of elapsed seconds since the R process was
    #'       started (from `proc.time()["elapsed"]`).
    resources = function() {
      client <- .subset2(private, ".client")
      if (is.null(client)) {
        client <- ps::ps_handle()
      }
      type <- "client"
      pid <- ps::ps_pid(p = client)
      status <- ps::ps_status(p = client)
      rss <- .subset2(ps::ps_memory_info(p = client), "rss")
      elapsed <- proc.time()["elapsed"]
      dispatcher <- .subset2(private, ".dispatcher")
      row_names <- "1"
      if (!is.null(dispatcher) && ps::ps_is_running(p = dispatcher)) {
        type[2L] <- "dispatcher"
        pid[2L] <- ps::ps_pid(p = dispatcher)
        status[2L] <- ps::ps_status(p = dispatcher)
        rss[2L] <- .subset2(ps::ps_memory_info(p = dispatcher), "rss")
        row_names[2L] <- "2"
        elapsed[2L] <- elapsed
      }
      out <- list(
        type = type,
        pid = pid,
        status = status,
        rss = rss,
        elapsed = elapsed
      )
      attributes(out) <- list(
        names = names(out),
        class = c("tbl_df", "tbl", "data.frame"),
        row.names = row_names
      )
      out
    },
    #' @description Write resource consumption from `resources()` to
    #'   the `log_resources` file originally supplied to the client.
    #' @details When called from the controller as a side effect,
    #'   logging is throttled so it does not happen more
    #'   frequently than `seconds_interval` seconds.
    #'   The only exception is the explicit `log()` controller method.
    #'
    #'   The log file has one row per observation of a process,
    #'   including the current
    #'   R process ("client") and the `mirai` dispatcher. If the dispatcher
    #'   is not included in the output, it means the dispatcher process
    #'   is not running.
    #'   Columns include:
    #'     * `type`: the type of process (client or dispatcher)
    #'     * `pid`: the process ID.
    #'     * `status`: The process status (from `ps::ps_status()`).
    #'     * `rss`: resident set size (RSS). RS is the total memory held by
    #'       a process, including shared libraries which may also be
    #'       in use by other processes. RSS is obtained
    #'       from `ps::ps_memory_info()` and shown in bytes.
    #'     * `elapsed`: number of elapsed seconds since the R process was
    #'       started (from `proc.time()["elapsed"]`).
    #' @param throttle `TRUE` to throttle with interval `seconds_interval`
    #'   seconds to avoid overburdening the system when writing to the log
    #'   file. `FALSE` otherwise.
    #' @return `NULL` (invisibly). Writes to the log file if `log_resources`
    #'   was originally given.
    #'   The log file itself is in comma-separated values
    #'   (CSV) format which can be easily read by `readr::read_csv()`.
    #'   If `log_resources` is `NULL`,
    #'   then `log()` has no effect.
    log = function(throttle = FALSE) {
      if (is.null(.subset2(private, ".throttle"))) {
        private$.throttle <- crew_throttle(
          seconds_interval = private$.seconds_interval
        )
      }
      if (throttle && !.subset2(.subset2(private, ".throttle"), "poll")()) {
        return(invisible())
      }
      path <- .subset2(private, ".log_resources")
      if (is.null(path)) {
        return(invisible())
      }
      if (!file.exists(dirname(path))) {
        dir.create(dirname(path), recursive = TRUE)
      }
      resources <- .subset2(self, "resources")()
      data.table::fwrite(x = resources, file = path, sep = ",", append = TRUE)
      invisible()
    }
  )
)
