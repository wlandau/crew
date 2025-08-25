#' @title Create a client object.
#' @export
#' @family client
#' @description Create an `R6` wrapper object to manage the `mirai` client.
#' @param name Deprecated on 2025-01-14 (`crew` version 0.10.2.9002).
#' @param workers Deprecated on 2025-01-13 (`crew` version 0.10.2.9002).
#' @param host IP address of the `mirai` client to send and receive tasks.
#'   If `NULL`, the host defaults to `nanonext::ip_addr()[1]`.
#' @param port TCP port to listen for the workers. If `NULL`,
#'   then an available ephemeral port is automatically chosen.
#'   Controllers running simultaneously on the same computer
#'   (as in a controller group) must not share the same TCP port.
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
#' @param serialization Either `NULL` (default) or an object produced by
#'   [mirai::serial_config()] to control the serialization
#'   of data sent to workers. This can help with either more efficient
#'   data transfers or to preserve attributes of otherwise
#'   non-exportable objects (such as `torch` tensors or `arrow` tables).
#'   See `?mirai::serial_config` for details.
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
  serialization = NULL,
  seconds_interval = 1,
  seconds_timeout = 60,
  retry_tasks = NULL
) {
  crew_deprecate(
    name = "name (in crew_client())",
    date = "2023-01-14",
    version = "0.10.2.9002",
    alternative = "none",
    value = name,
    condition = "message"
  )
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
  host <- as.character(host %|||% utils::head(nanonext::ip_addr(), n = 1L))
  crew_assert(
    host,
    is.character(.),
    length(.) == 1L,
    nzchar(.),
    !anyNA(.),
    message = paste0(
      "invalid host: ",
      host,
      ". In {crew} controllers, the `host` argument should have the ",
      "(local) IP address or host name of the local machine. ",
      "The default value of nanonext::ip_addr()[1] is usually enough ",
      "for most situations."
    )
  )
  port <- as.integer(port %|||% 0L)
  crew_assert(
    inherits(tls, "crew_class_tls"),
    message = "argument tls must be an object created by crew_tls()"
  )
  client <- crew_class_client$new(
    host = host,
    port = port,
    tls = tls,
    serialization = serialization,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout,
    relay = crew_relay(
      throttle = crew_throttle(seconds_max = seconds_interval)
    )
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
  portable = FALSE,
  private = list(
    .host = NULL,
    .port = NULL,
    .tls = NULL,
    .serialization = NULL,
    .seconds_interval = NULL,
    .seconds_timeout = NULL,
    .relay = NULL,
    .started = FALSE,
    .url = NULL,
    .profile = NULL,
    .condition = NULL,
    .client = NULL, # TODO: remove if/when the dispatcher becomes a thread.
    .dispatcher = NULL, # TODO: remove if/when the dispatcher becomes a thread.
    # TODO: remove if/when mirai::status()$events
    # becomes irrelevant (issue #232):
    .queue_events = NULL
  ),
  active = list(
    #' @field host See [crew_client()].
    host = function() {
      .host
    },
    #' @field port See [crew_client()].
    port = function() {
      .port
    },
    #' @field tls See [crew_client()].
    tls = function() {
      .tls
    },
    #' @field serialization See [crew_client()].
    serialization = function() {
      .serialization
    },
    #' @field seconds_interval See [crew_client()].
    seconds_interval = function() {
      .seconds_interval
    },
    #' @field seconds_timeout See [crew_client()].
    seconds_timeout = function() {
      .seconds_timeout
    },
    #' @field relay Relay object for event-driven programming on a downstream
    #'   condition variable.
    relay = function() {
      .relay
    },
    #' @field started Whether the client is started.
    started = function() {
      .started
    },
    #' @field url Client websocket URL.
    url = function() {
      .url
    },
    #' @field profile Compute profile of the client.
    profile = function() {
      .profile
    },
    #' @field condition Condition variable of the client.
    condition = function() {
      .condition
    },
    #' @field client Process ID of the local process running the client.
    client = function() {
      .client
    },
    #' @field dispatcher Process ID of the `mirai` dispatcher
    dispatcher = function() {
      .dispatcher
    }
  ),
  public = list(
    #' @description `mirai` client constructor.
    #' @return An `R6` object with the client.
    #' @param host Argument passed from [crew_client()].
    #' @param port Argument passed from [crew_client()].
    #' @param tls Argument passed from [crew_client()].
    #' @param serialization Argument passed from [crew_client()].
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
      host = NULL,
      port = NULL,
      tls = NULL,
      serialization = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL,
      relay = NULL
    ) {
      .host <<- host
      .port <<- port
      .tls <<- tls
      .serialization <<- serialization
      .seconds_interval <<- seconds_interval
      .seconds_timeout <<- seconds_timeout
      # Creating the CV here instead of as a default R6 field value
      # somehow appeases covr and R CMD check.
      .condition <<- nanonext::cv()
      .relay <<- relay
      .queue_events <<- collections::queue()
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        .host,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.),
        message = "host must be a valid nonempty character string."
      )
      crew_assert(
        .port,
        is.integer(.),
        length(.) == 1L,
        is.finite(.),
        . >= 0L,
        . <= 65535L,
        message = "port must be a single finite integer between 0 and 65536."
      )
      crew_assert(
        inherits(.tls, "crew_class_tls"),
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
          . >= 0,
          message = paste(
            field,
            "must be a valid nonempty non-missing number"
          )
        )
      }
      if_any(
        is.null(.client),
        NULL,
        crew_assert(inherits(.client, "ps_handle"))
      )
      if_any(
        is.null(.dispatcher),
        NULL,
        crew_assert(inherits(.dispatcher, "ps_handle"))
      )
      crew_assert(
        .seconds_timeout >= .seconds_interval,
        message = "seconds_timeout cannot be less than seconds_interval"
      )
      crew_assert(inherits(.relay, "crew_class_relay"))
      if_any(
        is.null(.serialization),
        NULL,
        crew_assert(is.list(.serialization))
      )
      .relay$validate()
      invisible()
    },
    #' @description Register the client as started.
    #' @details Exported to implement the sequential controller.
    #'   Only meant to be called manually inside the client or
    #'   the sequential controller.
    #' @return `NULL` (invisibly).
    set_started = function() {
      .started <<- TRUE
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    start = function() {
      if (!is.null(.started) && .started) {
        return(invisible())
      }
      .profile <<- crew_random_name()
      mirai::daemons(
        url = .tls$url(host = .host, port = .port),
        dispatcher = TRUE,
        seed = NULL,
        serial = .serialization,
        tls = .tls$client(),
        pass = .tls$password,
        .compute = .profile
      )
      .url <<- mirai::nextget("url", .compute = .profile)
      .client <<- ps::ps_handle()
      # TODO: remove code that gets the dispatcher PID if the dispatcher
      # process becomes a C thread.
      # Begin dispatcher code.
      pid <- mirai::nextget("pid", .compute = .profile)
      if (!is.null(pid)) {
        .dispatcher <<- ps::ps_handle(pid = pid)
      }
      # End dispatcher code.
      .condition <<- mirai::nextget(x = "cv", .compute = .profile)
      .relay$set_from(.condition)
      .relay$start()
      .started <<- TRUE
      .queue_events <<- collections::queue()
      invisible()
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (!isTRUE(.started)) {
        return(invisible())
      }
      if (!is.null(.profile)) {
        mirai::daemons(n = 0L, .compute = .profile)
      }
      .profile <<- NULL
      .condition <<- nanonext::cv()
      .relay$terminate()
      .url <<- NULL
      .started <<- FALSE
      # TODO: if the dispatcher process becomes a C thread,
      # delete these superfluous checks on the dispatcher.
      # Begin dispatcher checks.
      if (is.null(.dispatcher)) {
        return(invisible())
      }
      tryCatch(
        crew_retry(
          fun = ~ !ps::ps_is_running(p = .dispatcher),
          seconds_interval = .seconds_interval,
          seconds_timeout = .seconds_timeout
        ),
        error = function(condition) NULL
      )
      if_any(
        ps::ps_is_running(p = .dispatcher),
        try(
          crew_terminate_process(pid = ps::ps_pid(.dispatcher)),
          silent = TRUE
        ),
        NULL
      )
      tryCatch(
        crew_retry(
          fun = ~ !ps::ps_is_running(p = ps::ps_pid(.dispatcher)),
          seconds_interval = .seconds_interval,
          seconds_timeout = .seconds_timeout
        ),
        error = function(condition) NULL
      )
      # End dispatcher checks.
      .queue_events <<- collections::queue()
      invisible()
    },
    #' @description Get the counters from `mirai::status()`.
    #' @return A named integer vector of task counts
    #'   (awaiting, executing, completed) as well as the number of
    #'   worker connections.
    status = function() {
      status <- mirai_status(
        profile = .profile,
        seconds_interval = .seconds_interval,
        seconds_timeout = .seconds_timeout
      )
      push_events(.subset2(status, "events"))
      status
    },
    #' @description Collect mirai worker-specific
    #'   connection/disconnection events.
    #' @return `NULL` (invisibly).
    #' @param events Integer vector of connection/disconnection events.
    push_events = function(events) {
      # TODO: remove when issue #232 is fixed and crew no longer needs
      # mirai::status()$events.
      lapply(events, .subset2(.queue_events, "push"))
      invisible()
    },
    #' @description Collect mirai worker-specific
    #'   connection/disconnection events.
    #' @return Integer vector of connection/disconnection events.
    collect_events = function() {
      # TODO: remove when issue #232 is fixed and crew no longer needs
      # mirai::status()$events.
      as.integer(
        replicate(
          .subset2(.queue_events, "size")(),
          .subset2(.queue_events, "pop")()
        )
      )
    },
    #' @description Get the process IDs of the local process and the
    #'   `mirai` dispatcher (if started).
    #' @return An integer vector of process IDs of the local process and the
    #'   `mirai` dispatcher (if started).
    pids = function() {
      # TODO: remove this function if/when the dispatcher becomes a thread.
      out <- c(local = Sys.getpid())
      if (!is.null(.dispatcher)) {
        out <- c(out, ps::ps_pid(.dispatcher))
        names(out)[2L] <- paste0("dispatcher-", .profile)
      }
      out
    }
  )
)
