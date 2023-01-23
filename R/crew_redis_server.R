#' @title Redis server
#' @export
#' @family redis
#' @description Create a handle to configure, run, and manage
#'   an instance of Redis server.
#' @return An `R6` object of class [crew_class_redis_server]
#'   with methods to configure, run,
#'   and manage an instance of Redis server.
#' @param binary Nonempty non-missing character of length 1,
#'   path to the Redis server executable.
#'   The path must point to an existing file.
#'   If the `binary` argument is `NULL`,
#'   then the value defaults to `Sys.getenv("CREW_REDIS_SERVER_BINARY")`
#'   if this environment variable is set. Otherwise, the default becomes
#'   `unname(Sys.which("redis-server"))`.
#'   Visit <https://redis.io/> to learn how to install Redis server.
#' @param conf Non-missing character of length 1,
#'   path to the Redis server configuration file.
#'   The default value of the `conf` argument
#'   is `Sys.getenv("CREW_REDIS_SERVER_CONF")` if this environment
#'   variable is set. Otherwise, the default becomes
#'   `""` to use the system default configuration file.
#'   Visit <https://redis.io/docs/management/config/>
#'   to learn how to configure Redis.
#' @param host Nonempty non-missing character of length 1,
#'   IP address of the Redis server.
#'   The default value is `Sys.getenv("CREW_REDIS_SERVER_HOST")`,
#'   if this environment variable is set. Otherwise, the default value
#'   is the IPv4 loopback address `"127.0.0.1"`.
#' @param port Nonempty non-missing character of length 1,
#'   TCP port of the Redis server.
#'   The default value is `Sys.getenv("CREW_REDIS_SERVER_PORT")`
#'   if this environment variable is set. Otherwise, the default is
#'   a random ephemeral port.
#' @param password Non-missing character of length 1
#'   with at most 64 characters,
#'   password to the Redis server (default user).
#'   The default value is `Sys.getenv("CREW_REDIS_SERVER_PASSWORD")`
#'   if this environment variable is set. Otherwise, the default is a
#'   a random cryptographic 256-bit 64-character password.
#'   Visit <https://redis.io/docs/management/security/> to learn about
#'   Redis security.
#' @param start_timeout Positive numeric of length 1, number of seconds
#'   to wait for the Redis server to start up and begin accepting clients.
#' @param start_wait Positive numeric of length 1, polling interval
#'   (seconds) while waiting for the Redis server to start up and
#'   begin accepting clients.
#' @examples
#' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
#' server <- crew_redis_server()
#' server$validate()
#' server$test() # TRUE
#' server$start()
#' server$ping() # [Redis: PONG]
#' server$stop()
#' }
crew_redis_server <- function(
  binary = NULL,
  conf = NULL,
  host = NULL,
  port = NULL,
  password = NULL,
  start_timeout = 5,
  start_wait = 0.25
) {
  out <- crew_class_redis_server$new(
    binary = binary %|||% redis_server_default_binary(),
    conf = conf %|||% redis_server_default_conf(),
    host = host %|||% redis_server_default_host(),
    port = port %|||% redis_server_default_port(),
    password = password %|||% redis_server_default_password(),
    start_timeout = start_timeout,
    start_wait = start_wait
  )
  out$validate()
  out
}

#' @title Redis server class
#' @export
#' @family redis
#' @description `R6` class to create Redis server objects.
#' @details See [crew_redis_server()] for fields and default values.
crew_class_redis_server <- R6::R6Class(
  classname = "crew_class_redis_server",
  public = list(
    #' @field binary Path to the Redis server executable.
    binary = NULL,
    #' @field conf Path to the Redis configuration file.
    conf = NULL,
    #' @field host IP address of the Redis host.
    host = NULL,
    #' @field port TCP port of the Redis server.
    port = NULL,
    #' @field password Temporary session-specific password for Redis
    #'   authentication.
    password = NULL,
    #' @field start_timeout Number of seconds to wait for the Redis
    #'   server to start and get ready to accept connections from clients.
    start_timeout = NULL,
    #' @field start_wait Polling interval (seconds) to wait for the Redis
    #'   server to start and get ready to accept connections from clients.
    start_wait = NULL,
    #' @field process `processx::process` object with the Redis server process.
    process = NULL,
    #' @description Redis server object constructor.
    #' @return An `R6` object handle to a Redis server.
    #' @param binary Path to the Redis server executable.
    #' @param conf Path to the Redis configuration file.
    #' @param host IP address of the Redis host.
    #' @param port TCP port of the Redis server.
    #' @param password Temporary session-specific password
    #' @param start_timeout Seconds to wait for Redis to fully start.
    #' @param start_wait Seconds to poll for the Redis to fully start.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$validate()
    #' server$test() # TRUE
    #' server$start()
    #' server$ping() # [Redis: PONG]
    #' server$stop()
    #' }
    initialize = function(
      binary = NULL,
      conf = NULL,
      host = NULL,
      port = NULL,
      password = NULL,
      start_timeout = NULL,
      start_wait = NULL
    ) {
      self$binary <- binary
      self$conf <- conf
      self$host <- host
      self$port <- port
      self$password <- password
      self$start_timeout <- start_timeout
      self$start_wait <- start_wait
    },
    #' @description Clean up a Redis server object
    #'   (stop the server at garbage collection).
    finalize = function() {
      self$stop()
    },
    #' @description Represent the Redis server connection details
    #'   as a concise string.
    #' @return A length-one string representation of the Redis server
    #'   connection details.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server(password = "random")
    #' server$serialize() # "host=127.0.0.1&port=54034&password=random"
    #' }
    serialize = function() {
      redis_server_serialize(self)
    },
    #' @description Create a `redux` Redis client object that
    #'   connects to the Redis server.
    #' @details The client runs on the same host as the server.
    #'   This method is intended for testing and exploratory
    #'   purposes only.
    #' @return A `redux::hiredis()` object that can send
    #'   transactions to the Redis server.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$start()
    #' client <- server$client()
    #' client$PING() # [Redis: PONG]
    #' server$stop()
    #' }
    client = function() {
      redis_server_client(self)
    },
    #' @description Start a Redis server process.
    #' @details The Redis server object will only run one Redis
    #'   process at a time. If you call `start()` when the server is
    #'   already running, no new process will be created.
    #' @return `NULL` (invisibly).
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$start()
    #' server$alive() # TRUE
    #' server$stop()
    #' }
    start = function() {
      redis_server_start(self)
    },
    #' @description Stop the active Redis server process.
    #' @return `NULL` (invisibly).
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$start()
    #' server$alive() # TRUE
    #' server$stop()
    #' server$alive() # FALSE
    #' }
    stop = function() {
      redis_server_stop(self)
    },
    #' @description Check if the Redis server is running.
    #' @return `TRUE` if the Redis server process is running,
    #'   `FALSE` otherwise. If the Redis server is just starting,
    #'   then the server may not be ready for new clients
    #'   even though `alive()` returns `TRUE`. The `ready()`
    #'   method will detect if the server can accept connections.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$start()
    #' server$alive() # TRUE
    #' server$stop()
    #' server$alive() # FALSE
    #' }
    alive = function() {
      redis_server_alive(self)
    },
    #' @description Check if the Redis server is running and ready
    #'   to accept connections from clients.
    #' @return `TRUE` if the Redis server is running and ready to
    #'   accept connections from clients. `FALSE` otherwise.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$ready() # FALSE
    #' server$start()
    #' server$ready() # TRUE
    #' server$ping() # [Redis: PONG]
    #' server$stop()
    #' }
    ready = function() {
      redis_server_ready(self)
    },
    #' @description Ping the Redis server.
    #' @details Send a short transaction to test the connection
    #'   between the server and a new temporary client.
    #' @return An S3 object of class `"redis_status"` with value `"PONG"`.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$start()
    #' server$ping() # [Redis: PONG]
    #' server$stop()
    #' }
    ping = function() {
      redis_server_ping(self)
    },
    #' @description Test the Redis server.
    #' @details Start the Redis server, test it on a new client with
    #'   a ping, then stop the server.
    #' @return `TRUE` if the test succeeded, `FALSE` if there were issues.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$test() # TRUE
    #' }
    test = function() {
      redis_server_test(self)
    },
    #' @description Validate the Redis server object.
    #' @details This method does not actually start the Redis server,
    #'   so the check is superficial, quick, and non-invasive.
    #' @return Nothing (invisibly). This method throws an error
    #'   if it detects any configuration problems.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$validate() # silent
    #' }
    validate = function() {
      redis_server_validate(self)
    },
    #' @description Check that the fields of the `R6` object are valid.
    #' @details This method does not actually start the Redis server,
    #'   so the check is superficial, quick, and non-invasive.
    #' @return `TRUE` if the Redis server object is configured correctly
    #'   and `FALSE` otherwise.
    #' @examples
    #' if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
    #' server <- crew_redis_server()
    #' server$configured() # TRUE
    #' }
    configured = function() {
      redis_server_configured(self)
    }
  )
)

redis_server_default_binary <- function() {
  Sys.getenv(
    "CREW_REDIS_SERVER_BINARY",
    unset = unname(Sys.which("redis-server"))
  )
}

redis_server_default_conf <- function() {
  out <- Sys.getenv("CREW_REDIS_SERVER_CONF", unset = "")
  if_any(nzchar(out), out, NULL)
}

redis_server_default_host <- function() {
  Sys.getenv("CREW_REDIS_SERVER_HOST", unset = "127.0.0.1")
}

redis_server_default_port <- function() {
  Sys.getenv("CREW_REDIS_SERVER_PORT", unset = redis_server_random_port())
}

redis_server_random_port <- function(lower = 49152L, upper = 65535L) {
  ports <- seq.int(from = lower, to = upper, by = 1L)
  as.character(sample(ports, size = 1L))
}

redis_server_default_password <- function() {
  Sys.getenv(
    "CREW_REDIS_SERVER_PASSWORD",
    unset = redis_server_random_password()
  )
}

redis_server_random_password <- function() {
  digest::digest(
    object = openssl::rand_bytes(n = 256),
    algo = "sha256",
    serialize = FALSE
  )
}

redis_server_serialize <- function(self) {
  fields <- c("host", "port", "password")
  values <- map_chr(fields, ~self[[.x]])
  pairs <- paste(fields, values, sep = "=")
  paste(pairs, collapse = "&")
}

redis_server_start <- function(self) {
  if (!self$alive()) {
    self$process <- redis_server_process(
      binary = self$binary,
      conf = self$conf,
      host = self$host,
      port = self$port,
      password = self$password
    )
  }
  crew_wait(
    fun = self$alive,
    timeout = self$start_timeout,
    wait = self$start_wait,
    message = paste(
      c(
        "Redis server could not start.\n",
        self$process$read_all_output(),
        self$process$read_all_error()
      ),
      collapse = ""
    )
  )
  crew_wait(
    fun = self$ready,
    timeout = self$start_timeout,
    wait = self$start_wait,
    message = "Redis server could not initialize or receive connections."
  )
  invisible()
}

redis_server_process <- function(
  binary,
  conf,
  host,
  port,
  password
) {
  args <- c(
    conf,
    "--bind",
    host,
    "--port",
    port,
    "--requirepass",
    password
  )
  processx::process$new(
    command = binary,
    args = args,
    stdout = "|",
    stderr = "|",
    cleanup = TRUE,
    supervise = TRUE
  )
}

redis_server_stop <- function(self) {
  if (!is.null(self$process)) {
    self$process$kill()
  }
  invisible()
}

redis_server_alive <- function(self) {
  (!is.null(self$process)) && (self$process$is_alive())
}

redis_server_client <- function(self) {
  crew_true(
    self$alive(),
    message = "Redis server must start before accepting clients."
  )
  redux::hiredis(
    host = self$host,
    port = self$port,
    password = self$password
  )
}

redis_server_ping <- function(self) {
  client <- self$client()
  client$PING()
}

redis_server_ready <- function(self) {
  tryCatch(
    identical(as.character(self$ping()), "PONG"),
    error = crew_condition_false
  )
}

redis_server_test <- function(self) {
  crew_true(!self$alive(), message = "Redis server already running.")
  on.exit(self$stop())
  try(self$start(), silent = TRUE)
  self$ready()
}

redis_server_validate <- function(self) {
  crew_true(
    self$conf %|||% "redis.conf",
    is.character(.),
    length(.) == 1L,
    !anyNA(.),
    message = "conf must be NULL, or a non-missing character of length 1."
  )
  for (field in c("binary", "host", "port", "password")) {
    message <- paste(
      field,
      "must be a non-missing non-empty character of length 1."
    )
    crew_true(
      self[[field]],
      is.character(.),
      length(.) == 1L,
      !anyNA(.),
      nzchar(.),
      message = message
    )
  }
  crew_true(
    as.integer(self$port),
    !anyNA(.),
    length(.) == 1L,
    . >= 0L,
    . <= 65535L,
    message = "invalid TCP port"
  )
  crew_true(
    nchar(self$password) <= 64L,
    message = "password must be at most 64 characters."
  )
  message <- sprintf("Redis binary %s not found.", self$binary)
  crew_true(file.exists(self$binary), message = message)
  if (!is.null(self$conf)) {
    message <- sprintf("Redis configuration file %s not found.", self$conf)
    crew_true(file.exists(self$conf), message = message)
  }
  invisible()
}

redis_server_configured <- function(self) {
  tryCatch(
    redis_server_validate(self) %|||% TRUE,
    error = crew_condition_false
  )
}
