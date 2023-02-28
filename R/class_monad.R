monad_init <- function(
  name = NA_character_,
  command = NA_character_,
  result = NA,
  seconds = NA_real_,
  error = NA_character_,
  traceback = NA_character_,
  warnings = NA_character_
) {
  out <- monad_new(
    name = name %|||% NA_character_,
    command = command %|||% NA_character_,
    result = result %|||% NA,
    seconds = seconds %|||% NA_real_,
    error = error %|||% NA_character_,
    traceback = traceback %|||% NA_character_,
    warnings = warnings %|||% NA_character_
  )
  monad_validate(out)
  out
}

monad_new <- function(
  name = NULL,
  command = NULL,
  result = NULL,
  seconds = NULL,
  error = NULL,
  traceback = NULL,
  warnings = NULL
) {
  out <- list(
    name = name,
    command = command,
    result = list(result),
    seconds = seconds,
    error = error,
    traceback = traceback,
    warnings = warnings
  )
  tibble::new_tibble(x = out, class = "crew_monad")
}

monad_validate <- function(monad) {
  true(inherits(monad, "crew_monad"))
  true(tibble::is_tibble(monad))
  true(nrow(monad), 1L)
  true(identical(colnames(monad), names(formals(monad_new))))
  for (col in c("name", "command", "error", "traceback", "warnings")) {
    true(is.character(monad[[col]]))
  }
  true(is.numeric(monad$seconds))
  true(is.list(monad$result))
  invisible()
}
