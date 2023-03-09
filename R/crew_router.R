#' @title Create a router.
#' @export
#' @keywords internal
#' @family routers
#' @description Create an `R6` object to manage the `mirai` task scheduler
#'   client.
#' @param name Name of the router object. If `NULL`, a name is automatically
#'   generated.
#' @param workers Integer, maximum number of parallel workers to run.
#' @param host IP address of the client process that the workers can dial
#'   into inside the local network.
#'   If a character string, the router uses the specified IP address.
#'   If `NULL`, the IP address defaults to `getip::getip(type = "local")`.
#' @param port TCP port to listen for the workers. If `0`,
#'   then NNG automatically chooses an available ephemeral port.
#' @param router_timeout Number of seconds to time out waiting for the `mirai`
#'   client to (dis)connect.
#' @param router_wait Number of seconds to wait between iterations checking
#'   if the `mirai` client is (dis)connected.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_router()
#' router$unoccupied() # character(0)
#' router$listen()
#' router$unoccupied() # "ws://xx.xx.xx:xxxxx/x"
#' router$terminate()
#' router$unoccupied() # character(0)
#' }
crew_router <- function(
  name = NULL,
  workers = 1L,
  host = getip::getip(type = "local"),
  port = 0L,
  router_timeout = 5,
  router_wait = 0.1
) {
  true(workers, is.numeric(.), length(.) == 1L, . > 0L, !anyNA(.))
  name <- as.character(name %|||% random_name())
  workers <- as.integer(workers)
  host <- as.character(host)
  port <- as.integer(port)
  true(name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  true(workers, is.integer(.), length(.) == 1L, !anyNA(.), . > 0L)
  true(host, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  true(port, is.integer(.), length(.) == 1L, !anyNA(.))
  true(port, . >= 0L, . <= 65535L)
  true(router_timeout, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_wait, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_timeout >= router_wait)
  router <- crew_class_router$new(
    name = name,
    workers = workers,
    host = host,
    port = port,
    router_timeout = router_timeout,
    router_wait = router_wait
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
#' router$unoccupied() # character(0)
#' router$listen()
#' router$unoccupied() # "ws://xx.xx.xx:xxxxx/x"
#' router$terminate()
#' router$unoccupied() # character(0)
#' }
crew_class_router <- R6::R6Class(
  classname = "crew_class_router",
  cloneable = FALSE,
  public = list(
    #' @field name Name of the router.
    name = NULL,
    #' @field workers Number of workers.
    workers = NULL,
    #' @field host Local IP address.
    host = NULL,
    #' @field port Local TCP port.
    port = NULL,
    #' @field router_timeout Timeout in seconds for checking the `mirai`
    #'   client connection.
    router_timeout = NULL,
    #' @field router_wait Polling interval in seconds for checking the
    #'   `mirai` client connection.
    router_wait = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Argument passed from [crew_router()].
    #' @param workers Argument passed from [crew_router()].
    #' @param host Argument passed from [crew_router()].
    #' @param port Argument passed from [crew_router()].
    #' @param router_timeout Argument passed from [crew_router()].
    #' @param router_wait Argument passed from [crew_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router()
    #' router$unoccupied() # character(0)
    #' router$listen()
    #' router$unoccupied() # "ws://xx.xx.xx:xxxxx/x"
    #' router$terminate()
    #' router$unoccupied() # character(0)
    #' }
    initialize = function(
      name = NULL,
      workers = NULL,
      host = NULL,
      port = NULL,
      router_timeout = NULL,
      router_wait = NULL
    ) {
      self$name <- name
      self$workers <- workers
      self$host <- host
      self$port <- port
      self$router_timeout <- router_timeout
      self$router_wait <- router_wait
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$workers, is.integer(.), length(.) == 1L, !anyNA(.), . > 0L)
      true(self$host, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$port, is.integer(.), length(.) == 1L, !anyNA(.))
      true(self$port, . >= 0L, . <= 65535L)
      true(
        self$router_timeout,
        is.numeric(.),
        length(.) == 1L,
        !is.na(.), . >= 0
      )
      true(self$router_wait, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
      true(self$router_timeout >= self$router_wait)
      invisible()
    },
    #' @description Get the full matrix of worker nodes.
    #' @return Named integer matrix from `mirai::daemons()`
    nodes = function() {
      mirai::daemons(.compute = self$name)$nodes
    },
    #' @description Get all worker sockets.
    #' @return Character vector of websockets sockets that the
    #'   `mirai` client is currently listening to.
    sockets = function() {
      router_nodes_sockets(self$nodes())
    },
    #' @description Get the sockets of connected workers.
    #' @return Character vector of worker websockets.
    connected = function() {
      router_nodes_connected(self$nodes())
    },
    #' @description Get the sockets of disconnected workers.
    #' @return Character vector of worker websockets.
    disconnected = function() {
      router_nodes_disconnected(self$nodes())
    },
    #' @description Get the sockets of busy workers.
    #' @return Character vector of worker websockets.
    busy = function() {
      router_nodes_busy(self$nodes())
    },
    #' @description Get the sockets of idle workers.
    #' @return Character vector of worker websockets.
    #' @param nodes A named integer matrix from `mirai::daemons()$nodes`.
    idle = function() {
      router_nodes_idle(self$nodes())
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
      out <- mirai::daemons(.compute = self$name)$connections
      (length(out) == 1L) && !anyNA(out) && is.numeric(out) && out > 0L
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    listen = function() {
      if (isFALSE(self$listening())) {
        args <- list(
          value = sprintf("ws://%s:%s", self$host, self$port),
          nodes = self$workers,
          .compute = self$name
        )
        do.call(what = mirai::daemons, args = args)
        crew_wait(
          fun = ~isTRUE(self$listening()),
          timeout = self$router_timeout,
          wait = self$router_wait,
          message = "mirai client cannot connect."
        )
      }
      invisible()
    },
    #' @description Stop the mirai client and disconnect from the
    #'   worker websockets.
    #' @return `NULL` (invisibly).
    terminate = function() {
      if (isTRUE(self$listening())) {
        try(mirai::daemons(value = 0L, .compute = self$name), silent = TRUE)
        try(
          crew_wait(
            fun = ~isFALSE(self$listening()),
            timeout = self$router_timeout,
            wait = self$router_wait,
            message = "mirai client could not terminate."
          ),
          silent = TRUE
        )
      }
      invisible()
    }
  )
)

router_nodes_sockets <- function(nodes) {
  if (anyNA(nodes)) {
    return(character(0))
  }
  rownames(nodes)
}

router_nodes_connected <- function(nodes) {
  if (anyNA(nodes)) {
    return(character(0))
  }
  status_online <- nodes[, "status_online", drop = TRUE]
  connected <- status_online > 0L
  worker_sockets <- rownames(nodes)
  worker_sockets[connected]
}

router_nodes_disconnected <- function(nodes) {
  if (anyNA(nodes)) {
    return(character(0))
  }
  status_online <- nodes[, "status_online", drop = TRUE]
  disconnected <- status_online < 1L
  worker_sockets <- rownames(nodes)
  worker_sockets[disconnected]
}

router_nodes_busy <- function(nodes) {
  if (anyNA(nodes)) {
    return(character(0))
  }
  status_online <- nodes[, "status_online", drop = TRUE]
  status_busy <- nodes[, "status_busy", drop = TRUE]
  tasks_assigned <- nodes[, "tasks_assigned", drop = TRUE]
  tasks_complete <- nodes[, "tasks_complete", drop = TRUE]
  connected <- status_online > 0L
  busy <- status_busy > 0L
  backlogged <- (tasks_assigned - tasks_complete) > 0L
  worker_sockets <- rownames(nodes)
  worker_sockets[connected & (busy | backlogged)]
}

router_nodes_idle <- function(nodes) {
  if (anyNA(nodes)) {
    return(character(0))
  }
  status_online <- nodes[, "status_online", drop = TRUE]
  status_busy <- nodes[, "status_busy", drop = TRUE]
  tasks_assigned <- nodes[, "tasks_assigned", drop = TRUE]
  tasks_complete <- nodes[, "tasks_complete", drop = TRUE]
  connected <- status_online > 0L
  busy <- status_busy > 0L
  backlogged <- (tasks_assigned - tasks_complete) > 0L
  worker_sockets <- rownames(nodes)
  worker_sockets[connected & !busy & !backlogged]
}
