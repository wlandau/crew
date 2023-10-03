#' @title Create a client object.
#' @export
#' @family developer
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
#'   synchronous operations to complete. If `space_poll` is `TRUE`, then
#'   this is also the minimum number of seconds between calls to
#'   `mirai::daemons()` for the purposes of checking worker status.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' client <- crew_client()
#' client$start()
#' client$log()
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
  seconds_interval = 0.25,
  seconds_timeout = 10
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
    seconds_timeout = seconds_timeout
  )
  client$validate()
  client
}

#' @title `R6` client class.
#' @export
#' @family class
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
  public = list(
    #' @field name See [crew_client()].
    name = NULL,
    #' @field workers See [crew_client()].
    workers = NULL,
    #' @field host See [crew_client()].
    host = NULL,
    #' @field port See [crew_client()].
    port = NULL,
    #' @field tls See [crew_client()].
    tls = NULL,
    #' @field seconds_interval See [crew_client()].
    seconds_interval = NULL,
    #' @field seconds_timeout See [crew_client()].
    seconds_timeout = NULL,
    #' @field started Whether the client is started.
    started = NULL,
    #' @field dispatcher Process ID of the `mirai` dispatcher
    dispatcher = NULL,
    #' @description `mirai` client constructor.
    #' @return An `R6` object with the client.
    #' @param name Argument passed from [crew_client()].
    #' @param workers Argument passed from [crew_client()].
    #' @param host Argument passed from [crew_client()].
    #' @param port Argument passed from [crew_client()].
    #' @param tls Argument passed from [crew_client()].
    #' @param seconds_interval Argument passed from [crew_client()].
    #' @param seconds_timeout Argument passed from [crew_client()].
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
      seconds_timeout = NULL
    ) {
      self$name <- name
      self$workers <- workers
      self$host <- host
      self$port <- port
      self$tls <- tls
      self$seconds_interval <- seconds_interval
      self$seconds_timeout <- seconds_timeout
    },
    #' @description Validate the client.
    #' @return `NULL` (invisibly).
    validate = function() {
      crew_assert(
        self$name,
        is.character(.),
        length(.) == 1L,
        nzchar(.),
        !anyNA(.)
      )
      crew_assert(
        self$workers,
        is.integer(.),
        length(.) == 1L,
        !anyNA(.), . > 0L
      )
      crew_assert(
        self$host,
        is.character(.),
        length(.) == 1L,
        !anyNA(.),
        nzchar(.)
      )
      crew_assert(self$port, is.integer(.), length(.) == 1L, !anyNA(.))
      crew_assert(self$port, . >= 0L, . <= 65535L)
      crew_assert(
        inherits(self$tls, "crew_class_tls"),
        message = "argument tls must be an object created by crew_tls()"
      )
      fields <- c(
        "seconds_interval",
        "seconds_timeout"
      )
      for (field in fields) {
        crew_assert(
          self[[field]],
          is.numeric(.),
          length(.) == 1L,
          !is.na(.),
          . >= 0
        )
      }
      crew_assert(
        self$dispatcher %|||% 0L,
        is.numeric(.),
        length(.) == 1L,
        !is.na(.),
        . >= 0
      )
      crew_assert(self$seconds_timeout >= self$seconds_interval)
      invisible()
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    start = function() {
      started <- .subset2(self, "started")
      if (is.null(started)) {
        started <- FALSE
      }
      if (started) {
        return(invisible())
      }
      url <- sprintf(
        "%s://%s:%s",
        if_any(self$tls$mode == "none", "ws", "wss"),
        self$host,
        self$port
      )
      mirai::daemons(
        n = self$workers,
        url = url,
        dispatcher = TRUE,
        seed = NULL,
        tls = self$tls$client(),
        pass = self$tls$password,
        token = TRUE,
        .compute = self$name
      )
      # TODO: remove code that gets the dispatcher PID if the dispatcher
      # process becomes a C thread.
      # Begin dispatcher code.
      self$dispatcher <- mirai::nextget("pid", .compute = self$name)
      # End dispatcher code.
      self$started <- TRUE
      invisible()
    },
    #' @description Get the `nanonext` condition variable.
    #' @return The `nanonext` condition variable which tasks signal
    #'   on resolution. The return value is `NULL` if the client
    #'   is not running.
    condition = function() {
      mirai::nextget(x = "cv", .compute = .subset2(self, "name"))
    },
    #' @description Get the true value of the `nanonext` condition variable.
    #' @details Subtracts a safety offset which was padded on start.
    #' @return The value of the `nanonext` condition variable.
    condition_value = function() {
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
      if (!isTRUE(self$started)) {
        return(NULL)
      }
      daemons <- daemons_info(name = self$name)
      tibble::tibble(
        worker = seq_len(nrow(daemons)),
        online = as.logical(daemons[, "online"] > 0L),
        instances = as.integer(abs(daemons[, "instance"])),
        assigned = as.integer(daemons[, "assigned"]),
        complete = as.integer(daemons[, "complete"]),
        socket = as.character(rownames(daemons))
      )
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (!isTRUE(self$started)) {
        return(invisible())
      }
      # TODO: if the dispatcher process becomes a C thread,
      # delete these superfluous checks on the dispatcher.
      # Begin dispatcher checks block 1/2.
      handle <- if_any(
        is.null(self$dispatcher),
        NULL,
        tryCatch(
          ps::ps_handle(pid = self$dispatcher),
          error = function(condition) NULL
        )
      )
      # End dispatcher checks block 1/2.
      mirai::daemons(n = 0L, .compute = self$name)
      self$started <- FALSE
      # Begin dispatcher checks block 2/2.
      if (is.null(self$dispatcher) || is.null(handle)) {
        return(invisible())
      }
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = handle),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout
        ),
        error = function(condition) NULL
      )
      if_any(
        ps::ps_is_running(p = handle),
        try(ps::ps_kill(p = handle), silent = TRUE),
        NULL
      )
      tryCatch(
        crew_retry(
          fun = ~!ps::ps_is_running(p = handle),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout
        ),
        error = function(condition) NULL
      )
      # End dispatcher checks block 2/2.
      invisible()
    }
  )
)
