#' @title Create a `mirai` router.
#' @export
#' @family routers
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
#' @param ports Optional integer vector of TCP port numbers, one for each port
#'   on the client where a worker (`mirai` server) may dial in and accept tasks.
#'   Supersedes `workers` if supplied.
#'   For a vector of random available ports,
#'   you can use `parallelly::freePort()`.
#'   Defaults to a vector of length `workers` from `parallelly::freePort()`
#'   in the ephemeral range (49152 to 65535).
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_mirai_router()
#' router$validate()
#' router$connect()
#' # Now launch workers with a mirai launcher.
#' router$disconnect()
#' }
crew_mirai_router <- function(
  name = NULL,
  workers = 1L,
  host = NULL,
  ports = NULL
) {
  router <- crew_class_mirai_router$new(
    name = as.character(name %|||% random_name()),
    host = as.character(host %|||% local_ipv4()),
    ports = as.integer(ports %|||% random_ports(n = workers))
  )
  router$validate()
  router
}

#' @title `mirai` router class
#' @export
#' @family routers
#' @description `R6` class for `mirai` routers.
#' @details See [crew_mirai_router()].
crew_class_mirai_router <- R6::R6Class(
  classname = "crew_class_mirai_router",
  cloneable = FALSE,
  public = list(
    #' @field name Name of the router.
    name = NULL,
    #' @field host Client IP address. See [crew_mirai_router()] for details.
    host = NULL,
    #' @field ports TCP ports. See [crew_mirai_router()] for details.
    ports = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param name Name of the router object.
    #' @param host Client IP address. See [crew_mirai_router()] for details.
    #' @param ports TCP ports. See [crew_mirai_router()] for details.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_mirai_router()
    #' router$validate()
    #' router$connect()
    #' # Now launch workers with a mirai launcher.
    #' router$disconnect()
    #' }
    initialize = function(
      name = NULL,
      host = NULL,
      ports = NULL
    ) {
      self$name <- name
      self$host <- host
      self$ports <- ports
      invisible()
    },
    #' @description Disconnect at garbage collection time.
    #' @return `NULL` (invisibly).
    finalize = function() {
      self$disconnect()
    },
    #' @description Validate the router.
    #' @return `NULL` (invisibly).
    validate = function() {
      true(self$name, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$host, is.character(.), length(.) == 1L, nzchar(.), !anyNA(.))
      true(self$ports, is.integer(.), length(.) > 0L, . > 0L, !anyNA(.))
      true(self$ports, . >= 0L, . <= 65535L)
      invisible()
    },
    #' @description Get the TCP socket of the router.
    #' @return Character string with the TCP socket of the router.
    sockets = function() {
      tcp_sockets(host = self$host, ports = self$ports)
    },
    #' @description Get an available TCP socket for launching a worker.
    #' @return Character string with an available TCP socket.
    available_socket = function() {
      utils::head(setdiff(self$sockets(), self$connections()), n = 1L)
    },
    #' @description Check if the router is connected.
    #' @return `TRUE` if successfully listening for dialed-in workers,
    #'   `FALSE` otherwise.
    connected = function() {
      info <- mirai::daemons(.compute = self$name)
      identical(info$daemons, "remote") &&
        !anyNA(info$nodes) &&
        !mirai::is_error_value(info$nodes)
    },
    #' @description Show worker connections.
    #' @return Character vector of TCP sockets where the client is
    #'   listening to currently dialed-in running workers.
    connections = function() {
      nodes <- mirai::daemons(.compute = self$name)$nodes
      if_any(
        mirai::is_error_value(nodes),
        character(0),
        as.character(names(nodes)[nodes > 0L])
      )
    },
    #' @description Start listening for workers on the available sockets.
    #' @return `NULL` (invisibly).
    connect = function() {
      if (!self$connected()) {
        tcp <- self$sockets()
        args <- list(value = tcp, nodes = length(tcp), .compute = self$name)
        do.call(what = mirai::daemons, args = args)
      }
      true(
        self$connected(),
        message = "mirai client cannot connect. Please try different ports."
      )
      invisible()
    },
    #' @description Disconnect the router.
    #' @return `NULL` (invisibly).
    disconnect = function() {
      try(mirai::daemons(value = 0L, .compute = self$name), silent = TRUE)
      invisible()
    }
  )
)
