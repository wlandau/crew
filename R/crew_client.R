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
#' @param tls_enable Logical of length 1, whether to use transport layer
#'   security (TLS) to secure connections between the client and workers.
#'   Only supported for `mirai` version 0.9.0.9020 and above and
#'   `nanonext` version 0.9.0.9034 and above.
#'   Uses an automatically generated one-time self-signed certificate by
#'   default. To guard against man-in-the-middle attacks, consider
#'   generating a one-time certificate yourself, requesting a trusted
#'   certificate authority (CA) to sign it, and then supplying the
#'   keys to the `tls_config` argument. Enabling TLS requires `mirai`
#'   version 0.9.0.9013 or above, and a `NULL` value for `tls_enable`
#'   will enable TLS if and only if the `mirai` version is sufficient.
#' @param tls_config Optional and only relevant if TLS is enabled
#'   (see the `tls_config` argument). The `tls_config` argument
#'   controls how transport layer security (TLS) is configured,
#'   and it is directly forwarded to the `tls` argument of
#'   `mirai::daemons()`. If `tls_config` is `NULL`,
#'   then `mirai` will generate a one-time
#'   self-signed certificate. This default approach is protects against
#'   the simplest attempts at packet sniffing, but it is still vulnerable
#'   to man-in-the-middle attacks. When greater security is required,
#'   consider generating a PEM-encoded certificate and associated
#'   private key yourself and using a trusted certificate authority (CA)
#'   to sign the former. The documentation of `mirai`, including the
#'   `tls` arguments of the `mirai::daemons()` and `mirai::server()`
#'    functions, has more details.
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
  tls_enable = FALSE,
  tls_config = NULL,
  seconds_interval = 0.25,
  seconds_timeout = 10
) {
  name <- as.character(name %|||% crew_random_name())
  workers <- as.integer(workers)
  host <- as.character(host %|||% getip::getip(type = "local"))
  port <- as.integer(port %|||% 0L)
  tls_package_check(tls_enable)
  client <- crew_class_client$new(
    name = name,
    workers = workers,
    host = host,
    port = port,
    tls_enable = tls_enable,
    tls_config = tls_config,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  client$validate()
  client
}

# TODO: remove when mirai > 0.9.0 and nanonext > 0.9.0 are on CRAN.
tls_package_check <- function(tls_enable) {
  mirai <- utils::compareVersion(
    as.character(utils::packageVersion("mirai")),
    "0.9.0.9027"
  ) >= 0L
  nanonext <- utils::compareVersion(
    as.character(utils::packageVersion("nanonext")),
    "0.9.0.9039"
  ) >= 0L
  if_any(
    tls_enable && !(mirai && nanonext),
    crew_error(
      paste(
        "tls_enable = TRUE requires mirai >= 0.9.0.9027",
        "and nanonext >= 0.9.0.9039."
      )
    ),
    NULL
  )
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
    #' @field tls_enable See [crew_client()].
    tls_enable = NULL,
    #' @field tls_config See [crew_client()].
    tls_config = NULL,
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
    #' @param tls_enable Argument passed from [crew_client()].
    #' @param tls_config Argument passed from [crew_client()].
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
      tls_enable = NULL,
      tls_config = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL
    ) {
      self$name <- name
      self$workers <- workers
      self$host <- host
      self$port <- port
      self$tls_enable <- tls_enable
      self$tls_config <- tls_config
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
      crew_assert(self$tls_enable, isTRUE(.) || isFALSE(.))
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
        if_any(self$tls_enable, "wss", "ws"),
        self$host,
        self$port
      )
      args <- list(
        n = self$workers,
        url = url,
        dispatcher = TRUE,
        tls = self$tls_config,
        token = TRUE,
        .compute = self$name
      )
      do.call(what = mirai::daemons, args = args)
      # TODO: remove code that gets the dispatcher PID if the dispatcher
      # process becomes a C thread.
      # Begin dispatcher code.
      self$dispatcher <- environment(mirai::daemons)$..[[self$name]]$pid
      # End dispatcher code.
      self$started <- TRUE
      invisible()
    },
    #' @description Show an informative worker log.
    #' @return A `tibble` with information on the workers, or `NULL`
    #'   if the client is not started. The `tibble` has 1 row
    #'   per worker and the following columns:
    #'   * `worker`: integer index of the worker.
    #'   * `online`: `TRUE` if the worker is online and connected to the
    #'     websocket URL, `FALSE` otherwise.
    #'   * `instances`: integer, number of instances of `mirai` servers
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
        instances = as.integer(daemons[, "instance"]),
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
