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
  retry_tasks = TRUE
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
    .relay = NULL,
    .started = NULL,
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
    #' @field relay Relay object for event-driven programming on a downstream
    #'   condition variable.
    relay = function() {
      .subset2(private, ".relay")
    },
    #' @field started Whether the client is started.
    started = function() {
      .subset2(private, ".started")
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
      crew_assert(
        private$.dispatcher %|||% 0L,
        is.numeric(.),
        length(.) == 1L,
        !is.na(.),
        . >= 0
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
      # TODO: remove code that gets the dispatcher PID if the dispatcher
      # process becomes a C thread.
      # Begin dispatcher code.
      private$.dispatcher <- mirai::nextget("pid", .compute = private$.name)
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
      # TODO: if the dispatcher process becomes a C thread,
      # delete these superfluous checks on the dispatcher.
      # Begin dispatcher checks block 1/2.
      handle <- if_any(
        is.null(private$.dispatcher),
        NULL,
        tryCatch(
          ps::ps_handle(pid = private$.dispatcher),
          error = function(condition) NULL
        )
      )
      # End dispatcher checks block 1/2.
      mirai::daemons(n = 0L, .compute = private$.name)
      private$.relay$terminate()
      private$.started <- FALSE
      # Begin dispatcher checks block 2/2.
      if (is.null(private$.dispatcher) || is.null(handle)) {
        return(invisible())
      }
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = handle),
          seconds_interval = private$.seconds_interval,
          seconds_timeout = private$.seconds_timeout
        ),
        error = function(condition) NULL
      )
      if_any(
        ps::ps_is_running(p = handle),
        try(
          crew_terminate_process(p = ps::ps_pid(handle)),
          silent = TRUE
        ),
        NULL
      )
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = handle),
          seconds_interval = private$.seconds_interval,
          seconds_timeout = private$.seconds_timeout
        ),
        error = function(condition) NULL
      )
      # End dispatcher checks block 2/2.
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
    }
  )
)
