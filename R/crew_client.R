#' @title Create a client object.
#' @export
#' @family client
#' @description Create an `R6` wrapper object to manage the `mirai` client.
#' @param name Name of the client object. If `NULL`, a name is automatically
#'   generated.
#' @param workers Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).
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
#' @param retry_tasks Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' client$summary()
#' client$terminate()
#' }
crew_client <- function(
  name = NULL,
  workers = NULL,
  host = NULL,
  port = NULL,
  tls = crew::crew_tls(),
  tls_enable = NULL,
  tls_config = NULL,
  seconds_interval = 0.5,
  seconds_timeout = 60,
  retry_tasks = NULL
) {
  crew_deprecate(
    name = "workers (in crew_client())",
    date = "2023-01-13",
    version = "0.10.2.9002",
    alternative = "none",
    value = workers,
    condition = "message"
  )
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
  crew_deprecate(
    name = "retry_tasks",
    date = "2023-01-13",
    version = "0.10.2.9002",
    alternative = "none",
    value = retry_tasks,
    condition = "message"
  )
  name <- as.character(name %|||% crew_random_name())
  host <- as.character(host %|||% getip::getip(type = "local"))
  port <- as.integer(port %|||% 0L)
  crew_assert(
    inherits(tls, "crew_class_tls"),
    message = "argument tls must be an object created by crew_tls()"
  )
  client <- crew_class_client$new(
    name = name,
    host = host,
    port = port,
    tls = tls,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
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
    .host = NULL,
    .port = NULL,
    .tls = NULL,
    .seconds_interval = NULL,
    .seconds_timeout = NULL,
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
    #' @param host Argument passed from [crew_client()].
    #' @param port Argument passed from [crew_client()].
    #' @param tls Argument passed from [crew_client()].
    #' @param seconds_interval Argument passed from [crew_client()].
    #' @param seconds_timeout Argument passed from [crew_client()].
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
      host = NULL,
      port = NULL,
      tls = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      relay = NULL
    ) {
      private$.name <- name
      private$.host <- host
      private$.port <- port
      private$.tls <- tls
      private$.seconds_interval <- seconds_interval
      private$.seconds_timeout <- seconds_timeout
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
        "%s://%s:%s/%s",
        if_any(private$.tls$mode == "none", "ws", "wss"),
        private$.host,
        private$.port,
        private$.name
      )
      mirai::daemons(
        url = url,
        dispatcher = TRUE,
        seed = NULL,
        tls = private$.tls$client(),
        pass = private$.tls$password,
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
          crew_terminate_process(pid = ps::ps_pid(private$.dispatcher)),
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
      mirai::nextget(x = "cv", .compute = .subset2(private, ".name"))
    },
    #' @description Get the true value of the `nanonext` condition variable.
    #' @return The value of the `nanonext` condition variable.
    resolved = function() {
      condition <- .subset2(self, "condition")()
      if (is.null(condition)) {
        0L
      } else {
        nanonext::cv_value(condition)
      }
    },
    #' @description Show an informative worker log.
    #' @return A `tibble` with one row and the following columns:
    #'   * `awaiting`: number of tasks waiting to start.
    #'   * `executing`: number of tasks currently running on workers.
    #'   * `completed`: number of tasks completed.
    #'   * `connections`: number of workers connected to the client.
    #'   * `url`: websocket URL of the client.
    summary = function() {
      if (!isTRUE(private$.started)) {
        return(NULL)
      }
      status <- mirai_status(
        name = private$.name,
        seconds_interval = private$.seconds_interval,
        seconds_timeout = private$.seconds_timeout
      )
      tibble::tibble(
        awaiting = as.integer(status$mirai["awaiting"]),
        executing = as.integer(status$mirai["executing"]),
        completed = as.integer(status$mirai["completed"]),
        connections = status$connections,
        url = status$daemons
      )
    },
    #' @description Return the client URL.
    #' @return Character string, client URL. Returns `NULL` if the
    #'   client has not started or has since terminated.
    url = function() {
      if (!isTRUE(private$.started)) {
        return(NULL)
      }
      mirai_status(
        name = private$.name,
        seconds_interval = private$.seconds_interval,
        seconds_timeout = private$.seconds_timeout
      )$daemons
    },
    #' @description Get the process IDs of the local process and the
    #'   `mirai` dispatcher (if started).
    #' @return An integer vector of process IDs of the local process and the
    #'   `mirai` dispatcher (if started).
    pids = function() {
      out <- c(local = Sys.getpid())
      dispatcher <- private$.dispatcher
      if (!is.null(dispatcher)) {
        out <- c(out, ps::ps_pid(dispatcher))
        names(out)[2L] <- paste0("dispatcher-", private$.name)
      }
      out
    }
  )
)
