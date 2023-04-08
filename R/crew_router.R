#' @title Create a router.
#' @export
#' @keywords internal
#' @family routers
#' @description Create an `R6` object to manage the `mirai` task scheduler
#'   client.
#' @param name Name of the router object. If `NULL`, a name is automatically
#'   generated.
#' @param workers Integer, maximum number of parallel workers to run.
#' @param host IP address of the `mirai` client to send and receive tasks.
#'   If `NULL`, the host defaults to the local IP address.
#' @param port TCP port to listen for the workers. If `NULL`,
#'   then an available ephemeral port is automatically chosen.
#' @param seconds_interval Number of seconds between
#'   polling intervals waiting for certain internal
#'   synchronous operations to complete.
#' @param seconds_timeout Number of seconds until timing
#'   out while waiting for certain synchronous operations to complete.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$listen()
#' router$daemons
#' router$terminate()
#' }
crew_router <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  port = NULL,
  seconds_interval = 0.01,
  seconds_timeout = 5
) {
  name <- as.character(name %|||% random_name())
  workers <- as.integer(workers)
  host <- as.character(host %|||% getip::getip(type = "local"))
  port <- as.integer(port %|||% 0L)
  router <- crew_class_router$new(
    name = name,
    workers = workers,
    host = host,
    port = port,
    seconds_interval = seconds_interval,
    seconds_timeout = seconds_timeout
  )
  router$validate()
  router
}

#' @title Router class
#' @export
#' @family routers
#' @description `R6` class for `mirai` routers.
#' @details See [crew_router()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$listen()
#' router$daemons
#' router$terminate()
#' }
crew_class_router <- R6::R6Class(
  classname = "crew_class_router",
  cloneable = FALSE,
  public = list(
    #' @field name See [crew_router()].
    name = NULL,
    #' @field workers See [crew_router()].
    workers = NULL,
    #' @field host See [crew_router()].
    host = NULL,
    #' @field port See [crew_router()].
    port = NULL,
    #' @field seconds_interval See [crew_router()].
    seconds_interval = NULL,
    #' @field seconds_timeout See [crew_router()].
    seconds_timeout = NULL,
    #' @field dispatcher Process ID of the `mirai` dispatcher
    dispatcher = NULL,
    #' @field daemons Data frame of information from `mirai::daemons()`.
    daemons = NULL,
    #' @field rotations Logical vector to keep track of rotated
    #'   worker websockets.
    rotations = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Argument passed from [crew_router()].
    #' @param workers Argument passed from [crew_router()].
    #' @param host Argument passed from [crew_router()].
    #' @param port Argument passed from [crew_router()].
    #' @param seconds_interval Argument passed from [crew_router()].
    #' @param seconds_timeout Argument passed from [crew_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' router$listen()
    #' router$daemons
    #' router$terminate()
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      host = NULL,
      port = NULL,
      seconds_interval = NULL,
      seconds_timeout = NULL
    ) {
      self$name <- name
      self$workers <- workers
      self$host <- host
      self$port <- port
      self$seconds_interval <- seconds_interval
      self$seconds_timeout <- seconds_timeout
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$workers, is.integer(.), length(.) == 1L, !anyNA(.), . > 0L)
      true(self$host, is.character(.), length(.) == 1L, !anyNA(.), nzchar(.))
      true(self$port, is.integer(.), length(.) == 1L, !anyNA(.))
      true(self$port, . >= 0L, . <= 65535L)
      fields <- c(
        "seconds_interval",
        "seconds_timeout"
      )
      for (field in fields) {
        true(
          self[[field]],
          is.numeric(.),
          length(.) == 1L,
          !is.na(.),
          . >= 0
        )
      }
      true(
        self$dispatcher %|||% 0L,
        is.numeric(.),
        length(.) == 1L,
        !is.na(.),
        . >= 0
      )
      true(self$seconds_timeout >= self$seconds_interval)
      true(self$daemons, is.null(.) || is.data.frame(.))
      invisible()
    },
    #' @description Check if the `mirai` client is listening
    #'   to worker websockets.
    #' @details This method may stall and time out if there are
    #'   tasks in the queue. Methods `listen()` and `terminate()`
    #'   call `listening()` to manage the connection before
    #'   and after the entire workload, respectively.
    #' @return `TRUE` if successfully listening for dialed-in workers,
    #'   `FALSE` otherwise.
    listening = function() {
      info <- mirai::daemons(.compute = self$name)
      connections <- info$connections
      n_daemons <- nrow(info$daemons)
      # TODO: when the dispatcher process becomes a C thread,
      # delete these superfluous checks on the dispatcher.
      # Begin dispatcher checks.
      dispatcher <- attr(dimnames(info$daemons)[[1]], "dispatcher_pid")
      dispatcher_running <- TRUE
      if (!is.null(dispatcher)) {
        handle <- ps::ps_handle(pid = dispatcher)
        dispatcher_running <- ps::ps_is_running(p = handle)
      }
      dispatcher_running &&
      # End dispatcher checks.
        (length(connections) == 1L) &&
        (length(n_daemons) == 1L) &&
        !anyNA(connections) &&
        !anyNA(n_daemons) &&
        is.numeric(connections) &&
        is.numeric(n_daemons) &&
        all(connections > 0L) &&
        all(n_daemons > 0L)
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    listen = function() {
      if (isFALSE(self$listening())) {
        args <- list(
          url = sprintf("ws://%s:%s", self$host, self$port),
          n = self$workers,
          dispatcher = TRUE,
          token = TRUE,
          .compute = self$name
        )
        do.call(what = mirai::daemons, args = args)
        crew_wait(
          fun = ~isTRUE(self$listening()),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout,
          message = "mirai client cannot connect."
        )
        self$poll()
        # TODO: remove code that gets the dispatcher PID if the dispatcher
        # process is phased out of mirai.
        # Begin dispatcher code.
        self$dispatcher <- attr(dimnames(self$daemons)[[1]], "dispatcher_pid")
        # End dispatcher code.
        self$rotations <- rep(-1L, nrow(self$daemons))
      }
      invisible()
    },
    #' @description Get the current socket addresses.
    #' @return A character vector of socket addresses.
    sockets = function() {
      as.character(rownames(mirai::daemons(.compute = self$name)$daemons))
    },
    #' @description Choose the websocket path for the next instance
    #'   of the worker at a given index.
    #' @details The first call to `route()` at a given index
    #'   uses the original websocket path provided by `mirai::daemons()`.
    #'   Subsequent calls to `route()` at the same index rotate
    #'   the websocket path for robustness.
    #' @param index Integer of length 1, worker index.
    #' @return Character of length 1, new websocket path of the worker.
    route = function(index) {
      rotations <- self$rotations[index]
      self$rotations[index] <- rotations + 1L
      if_any(
        rotations > -1L,
        mirai::saisei(i = index, .compute = self$name),
        rownames(self$daemons)[index]
      )
    },
    #' @description Update the `daemons` field with
    #'   information on the `mirai` daemons.
    #' @details Call `mirai::daemons()` to get information about the workers.
    #'   If the workers cannot be reached or the router has not started,
    #'   then do not modify the `daemons` field. Otherwise, if the workers
    #'   are reachable, populate the `daemons` field with a data frame
    #'   of high-level worker-specific statistics.
    #' @return `NULL` (invisibly).
    poll = function() {
      out <- mirai::daemons(.compute = self$name)$daemons
      if (is.matrix(out)) {
        self$daemons <- out
      }
      invisible()
    },
    #' @description Show an informative worker log.
    #' @return A `tibble` with information on the workers.
    log = function() {
      self$poll()
      daemons <- self$daemons
      if (is.null(daemons)) {
        return(NULL)
      }
      sockets <- as.character(rownames(daemons))
      tibble::tibble(
        tasks_assigned = as.integer(daemons[, "assigned"]),
        tasks_complete = as.integer(daemons[, "complete"]),
        worker_connected = as.logical(daemons[, "online"] > 0L),
        worker_instances = as.integer(daemons[, "instance"]),
        worker_socket = sockets
      )
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (isTRUE(self$listening())) {
        self$poll()
        daemons <- mirai::daemons(.compute = self$name)$daemons
        # TODO: when the dispatcher process becomes a C thread,
        # delete these superfluous checks on the dispatcher.
        # Begin dispatcher checks block 1/2.
        dispatcher <- attr(dimnames(daemons)[[1]], "dispatcher_pid")
        if (!is.null(dispatcher)) {
          handle <- ps::ps_handle(pid = dispatcher)
        }
        # End dispatcher checks block 1/2.
        mirai::daemons(n = 0L, .compute = self$name)
        crew_wait(
          fun = ~isFALSE(self$listening()),
          seconds_interval = self$seconds_interval,
          seconds_timeout = self$seconds_timeout,
          message = "mirai client could not terminate."
        )
        # Begin dispatcher checks block 2/2.
        if (!is.null(dispatcher)) {
          tryCatch(
            crew_wait(
              fun = ~!ps::ps_is_running(p = handle),
              seconds_interval = self$seconds_interval,
              seconds_timeout = self$seconds_timeout
            ),
            crew_expire = function(condition) NULL
          )
          if_any(
            ps::ps_is_running(p = handle),
            ps::ps_kill(p = handle),
            NULL
          )
          tryCatch(
            crew_wait(
              fun = ~!ps::ps_is_running(p = handle),
              seconds_interval = self$seconds_interval,
              seconds_timeout = self$seconds_timeout
            ),
            crew_expire = function(condition) NULL
          )
          if_any(
            ps::ps_is_running(p = handle),
            tools::pskill(pid = dispatcher, signal = tools::SIGQUIT),
            NULL
          )
          crew_wait(
            fun = ~!ps::ps_is_running(p = handle),
            seconds_interval = self$seconds_interval,
            seconds_timeout = self$seconds_timeout,
            message = paste("could not terminate dispatcher PID", dispatcher)
          )
        }
        # End dispatcher checks block 2/2.
        self$daemons <- NULL
        self$dispatcher <- NULL
        self$rotations <- NULL
      }
      invisible()
    }
  )
)
