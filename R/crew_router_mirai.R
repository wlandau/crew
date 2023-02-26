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
#' router <- crew_router_mirai()
#' router$validate()
#' router$connect()
#' # Now launch workers with a mirai launcher.
#' router$disconnect()
#' }
crew_router_mirai <- function(host = NULL, port = NULL) {
  router <- crew_class_router_mirai$new(
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
#' @details See [crew_router_mirai()].
crew_class_router_mirai <- R6::R6Class(
  classname = "crew_class_router_mirai",
  public = list(
    #' @field host IP address the router uses to connect to the workers.
    host = NULL,
    #' @field port TCP port the router uses to connect to the workers.
    port = NULL,
    #' @field tasks Data frame of tasks in the queue.
    tasks = tibble::tibble(
      name = character(0),
      pushed = character(0),
      command = character(0),
      handle = list()
    ),
    #' @description `mirai` router constructor.
    #' @return An `R6` object with the router.
    #' @param host Host of the router. See [crew_router_mirai()].
    #' @param port TCP port of the router. See [crew_router_mirai()].
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' router <- crew_router_mirai()
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
      crew_true(host, is.character(.), length(.) == 1L)
      crew_true(port, is.integer(.), length(.) == 1L, . > 0L)
      invisible()
    },
    #' @description Connect the router to the host and port.
    #' @return `NULL` (invisibly).
    connect = function() {
      mirai::daemons(value = tcp_socket(host = self$host, port = self$port))
      invisible()
    },
    #' @description Check if the router is connected.
    #' @return `TRUE` if connected and `FALSE` otherwise.
    connected <- function() {
      anyNA(mirai::daemons()$daemons)
    },
    #' @description Disconnect the router from the host and port.
    #' @return `NULL` (invisibly).
    disconnect = function() {
      mirai::daemons(value = 0L)
      invisible()
    }
    #' @description Return the number of workers connected to the router.
    #' @return Nonnegative integer, number of connected workers.
    connections = function() {
      mirai::daemons()$connections
    },
    #' @description Push a task to the head of the task list.
    #' @return `NULL` (invisibly).
    #' @inheritParams mirai::mirai
    #' @param command R code with the task to run.
    #' @param args A list of objects referenced in `expr`.
    #' @param timeout Time in milliseconds for the task.
    #' @param name Optional name of the task. Replaced with a random name
    #'   if `NULL` or in conflict with an existing name in the task list.
    push = function(command, args = list(), timeout = NULL, name = NULL) {
      message <- "router must be connected to push tasks."
      crew_true(self$connected(), message = message)
      while (is.null(name) || name %in% self$tasks$name) name <- ids::proquint()
      monad <- as.call(list(quote(crew::crew_monad), substitute(command)))
      pushed <- format(Sys.time(), "%z UTC %Y-%m-%d %H:%M %OS2")
      handle <- mirai::mirai(.expr = monad, .args = args, .timeout = timeout)
      tibble::add_row(
        self$tasks,
        name = name,
        pushed = pushed,
        command = deparse_safe(command),
        handle = handle,
        .before = 1L
      )
      invisible()
    },
    #' @description Pop a completed task from as near as possible to the tail
    #'   of the task list.
    #' @return If there is a completed task available to collect, the return
    #'   value is a list object with the results, warnings, and errors.
    #'   Otherwise, if there are no tasks or if all tasks are still running,
    #'   the return value is `NULL`.
    pop = function() {
      for (index in rev(seq_len(nrow(self$tasks)))) {
        task <- self$tasks$handle[[index]]
        if (!inherits(task$data), "unresolvedValue") {
          
          stop("Figure out clean return value")
          
          return(task$data)
        }
      }
      NULL
    }
  )
)
