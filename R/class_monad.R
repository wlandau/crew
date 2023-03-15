monad_init <- function(
  name = NA_character_,
  command = NA_character_,
  result = NA,
  seconds = NA_real_,
  seed = NA_integer_,
  error = NA_character_,
  traceback = NA_character_,
  warnings = NA_character_,
  socket_data = NA_character_,
  socket_session = NA_character_
) {
  out <- monad_new(
    name = name %|||% NA_character_,
    command = command %|||% NA_character_,
    result = result %|||% NA,
    seconds = seconds %|||% NA_real_,
    seed = seed %|||% NA_integer_,
    error = error %|||% NA_character_,
    traceback = traceback %|||% NA_character_,
    warnings = warnings %|||% NA_character_,
    socket_data = socket_data %|||% NA_character_,
    socket_session = socket_session %|||% NA_character_
  )
  monad_validate(out)
  out
}

monad_new <- function(
  name = NULL,
  command = NULL,
  result = NULL,
  seconds = NULL,
  seed = NULL,
  error = NULL,
  traceback = NULL,
  warnings = NULL,
  socket_data = NULL,
  socket_session = NULL
) {
  out <- list(
    name = name,
    command = command,
    result = list(result),
    seconds = seconds,
    seed = seed,
    error = error,
    traceback = traceback,
    warnings = warnings,
    socket_data = socket_data,
    socket_session = socket_session
  )
  tibble::new_tibble(x = out, class = "crew_monad")
}

monad_validate <- function(monad) {
  true(inherits(monad, "crew_monad"))
  true(tibble::is_tibble(monad))
  true(nrow(monad), 1L)
  true(identical(colnames(monad), names(formals(monad_new))))
  cols <- c(
    "name",
    "command",
    "error",
    "traceback",
    "warnings",
    "socket_data",
    "socket_session"
  )
  for (col in cols) {
    true(is.character(monad[[col]]))
  }
  for (col in c("seconds", "seed")) {
    true(is.numeric(monad[[col]]))
  }
  true(is.list(monad$result))
  invisible()
}
