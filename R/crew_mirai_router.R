#' @title Create a `mirai` router.
#' @export
#' @keywords internal
#' @family mirai
#' @description Create an `R6` object to route tasks to `mirai` workers
#'   on the local network.
#' @section Ports:
#'   In the `mirai`-based task scheduling in `crew`, each parallel worker
#'   dials into a different TCP port on the local machine.
#'   If you launch hundreds of workers, then hundreds of ports will
#'   not be available to other users or processes.
#'   Large numbers of workers on shared machines or clusters may
#'   seriously disrupt the tasks of other users, so please be careful.
#' @param name Name of the `mirai` router.
#'   Defaults to a string from `ids::proquint()`.
#' @param workers Integer, maximum number of parallel workers to run.
#'   `crew` will reserve one ephemeral port for each worker. See the
#'   Ports section for an important cautionary note.
#' @param host IP address of the client process that the workers can dial
#'   into inside the local network.
#'   If a character string, the router uses the specified IP address.
#'   If `NULL`, the IP address defaults to `getip::getip(type = "local")`.
#' @param ports Optional integer vector of TCP port numbers.
#'   Supersedes `workers` if supplied. Except for 0,
#'   which defers to NNG to automatically assign a port, each unique port
#'   value corresponds to a local port where a worker will dial in
#'   to accept tasks.
#' @param router_timeout Number of seconds to time out waiting for the `mirai`
#'   router to (dis)connect.
#' @param router_wait Number of seconds to wait between iterations checking
#'   if the `mirai` router is (dis)connected.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_mirai_router()
#' router$sockets_listening() # character(0)
#' router$connect()
#' router$sockets_listening() # "tcp://xx.xx.xx:xxxxx"
#' router$disconnect()
#' router$sockets_listening() # character(0)
#' }
crew_mirai_router <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  ports = NULL,
  router_timeout = 5,
  router_wait = 0.1
) {
  true(workers, is.numeric(.), length(.) == 1L, . > 0L, !anyNA(.))
  name <- as.character(name %|||% random_name())
  host <- as.character(host %|||% local_ipv4())
  ports <- as.integer(ports %|||% rep(0, workers))
  true(name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  true(host, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
  true(ports, is.integer(.), length(.) > 0L, !anyNA(.))
  true(ports, . >= 0L, . <= 65535L)
  true(router_timeout, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_wait, is.numeric(.), length(.) == 1L, !is.na(.), . >= 0)
  true(router_timeout >= router_wait)
  router <- crew_class_mirai_router$new(
    name = name,
    host = host,
    ports = ports,
    router_timeout = router_timeout,
    router_wait = router_wait
  )
  router$validate()
  router
}

#' @title `mirai` router class
#' @export
#' @family mirai
#' @description `R6` class for `mirai` routers.
#' @details See [crew_mirai_router()].
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_mirai_router()
#' router$sockets_listening() # character(0)
#' router$connect()
#' router$sockets_listening() # "tcp://xx.xx.xx:xxxxx"
#' router$disconnect()
#' router$sockets_listening() # character(0)
#' }
crew_class_mirai_router <- R6::R6Class(
  classname = "crew_class_mirai_router",
  cloneable = FALSE,
  public = list(
    #' @field name Name of the router.
    name = NULL,
    #' @field host Local IP address.
    host = NULL,
    #' @field ports TCP ports to listen to workers.
    ports = NULL,
    #' @field router_timeout Timeout in seconds
    #'   (dis)connecting the `mirai` router.
    router_timeout = NULL,
    #' @field router_wait Polling interval in seconds checking if the `mirai`
    #'   router successfully (dis)connected.
    router_wait = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Argument passed from [crew_mirai_router()].
    #' @param host Argument passed from [crew_mirai_router()].
    #' @param ports Argument passed from [crew_mirai_router()].
    #' @param router_timeout Argument passed from [crew_mirai_router()].
    #' @param router_wait Argument passed from [crew_mirai_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_mirai_router()
    #' router$sockets_listening() # character(0)
    #' router$connect()
    #' router$sockets_listening() # "tcp://xx.xx.xx:xxxxx"
    #' router$disconnect()
    #' router$sockets_listening() # character(0)
    #' }
    initialize = function(
      name = NULL,
      host = NULL,
      ports = NULL,
      router_timeout = NULL,
      router_wait = NULL
    ) {
      self$name <- name
      self$host <- host
      self$ports <- ports
      self$router_timeout <- router_timeout
      self$router_wait <- router_wait
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$host, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$ports, is.integer(.), length(.) > 0L, !anyNA(.))
      true(self$ports, . >= 0L, . <= 65535L)
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
    #' @description Get all sockets
    #' @return Character vector of all sockets if connected, or
    #'   `character(0)` if the router is not connected
    #'   or polling the sockets is unsuccessful
    #'   (which may happen if the sockets are busy at the moment).
    sockets_listening = function() {
      nodes <- mirai::daemons(.compute = self$name)$nodes
      if_any(
        mirai::is_error_value(nodes),
        character(0),
        as.character(names(nodes))
      )
    },
    #' @description Show worker connections.
    #' @return Character vector of TCP sockets where workers
    #'   are currently dialed in.
    #'   Returns `character(0)` if the router is not connected
    #'   or polling the sockets is unsuccessful
    #'   (which may happen if the sockets are busy at the moment).
    sockets_occupied = function() {
      nodes <- mirai::daemons(.compute = self$name)$nodes
      if_any(
        mirai::is_error_value(nodes),
        character(0),
        as.character(names(nodes)[nodes > 0L])
      )
    },
    #' @description Get TCP sockets that are available for workers to dial in.
    #' @return Character string with available TCP sockets.
    #'   A return value of `character(0)` may indicate that
    #'   the router is not connected
    #'   or polling the sockets is unsuccessful
    #'   (which may happen if the sockets are busy at the moment).
   sockets_available = function() {
      as.character(setdiff(self$sockets_listening(), self$sockets_occupied()))
    },
    #' @description Check if the router is connected.
    #' @details This method may stall and time out if there are
    #'   tasks in the queue. Methods `connect()` and `disconnect()`
    #'   call `is_connected()` to manage the connection before
    #'   and after the entire workload, respectively.
    #' @return `TRUE` if successfully listening for dialed-in workers,
    #'   `FALSE` otherwise.
    is_connected = function() {
      out <- mirai::daemons(.compute = self$name)$connections
      (length(out) == 1L) && !anyNA(out) && is.numeric(out) && out > 0L
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    connect = function() {
      if (isFALSE(self$is_connected())) {
        tcp <- tcp_sockets(host = self$host, ports = self$ports)
        args <- list(value = tcp, nodes = length(tcp), .compute = self$name)
        do.call(what = mirai::daemons, args = args)
        crew_wait(
          fun = ~isTRUE(self$is_connected()),
          timeout = self$router_timeout,
          wait = self$router_wait,
          message = "mirai client cannot connect."
        )
      }
      invisible()
    },
    #' @description Disconnect the router.
    #' @return `NULL` (invisibly).
    disconnect = function() {
      if (isTRUE(self$is_connected())) {
        try(mirai::daemons(value = 0L, .compute = self$name), silent = TRUE)
        try(
          crew_wait(
            fun = ~isFALSE(self$is_connected()),
            timeout = self$router_timeout,
            wait = self$router_wait,
            message = "mirai client cannot disconnect."
          ),
          silent = TRUE
        )
      }
      invisible()
    }
  )
)

#' @export
#' @keywords internal
is_router.crew_class_mirai_router <- function(x) {
  TRUE
}
