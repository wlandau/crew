#' @title Create a `mirai` router.
#' @export
#' @family routers
#' @description Create an `R6` object to route tasks to `mirai` workers
#'   on the local network.
#' @param host IP address the router uses to connect to the workers.
#'   If a character string, the router uses the specified IP address.
#'   If `NULL`, the IP address defaults to `getip::getip(type = "local")`.
#' @param port TCP port the router uses to connect to the workers.
#'   If an integer or character string, the router uses the specified port.
#'   If `NULL`, the port defaults to an available port in the ephemeral
#'   port range (49152 through 65355).
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' router <- crew_mirai_router()
#' router$validate()
#' router$connect()
#' # Now launch workers with a mirai launcher.
#' router$disconnect()
#' }
crew_mirai_router <- function(host = NULL, port = NULL) {
  router <- crew_class_mirai_router$new(
    host = as.character(host %|||% local_ipv4()),
    port = as.integer(port %|||% random_port())
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
  public = list(
    #' @field host IP address the router uses to connect to the workers.
    host = NULL,
    #' @field port TCP port the router uses to connect to the workers.
    port = NULL,
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param host Host of the router. See [crew_mirai_router()].
    #' @param port TCP port of the router. See [crew_mirai_router()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_mirai_router()
    #' router$validate()
    #' router$connect()
    #' # Now launch workers with a mirai launcher.
    #' router$disconnect()
    #' }
    initialize = function(
      host = NULL,
      port = NULL
    ) {
      self$host <- host
      self$port <- port
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
      true(host, is.character(.), length(.) == 1L)
      true(port, is.integer(.), length(.) == 1L, . > 0L)
      invisible()
    },
    #' @description Check if the router is connected.
    #' @return `TRUE` if connected and `FALSE` otherwise.
    connected = function() {
      anyNA(mirai::daemons()$daemons)
    },
    #' @description Return the number of workers connected to the router.
    #' @return Nonnegative integer, number of connected workers.
    connections = function() {
      mirai::daemons()$connections
    },
    #' @description Connect the router to the host and port.
    #' @return `NULL` (invisibly).
    connect = function() {
      if (!self$connected()) {
        mirai::daemons(value = tcp_socket(host = self$host, port = self$port))
      }
      invisible()
    },
    #' @description Disconnect the router from the host and port.
    #' @return `NULL` (invisibly).
    disconnect = function() {
      mirai::daemons(value = 0L)
      invisible()
    }
  )
)
