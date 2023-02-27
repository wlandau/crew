monad_init <- function(
  name = NA_character_,
  command = NA_character_,
  result = list(crew_monad_no_result),
  runtime = NA_real_,
  overhead = NA_real_,
  error = NA_character_,
  traceback = NA_character_,
  warnings = NA_character_
) {
  monad_new(
    name = name,
    command = command,
    result = result,
    runtime = runtime,
    overhead = overhead,
    error = error,
    traceback = traceback,
    warnings = warnings
  )
}

monad_new <- function(
  name,
  command,
  result,
  runtime,
  overhead,
  error,
  traceback,
  warnings
) {
  out <- list(
    name = name,
    command = command,
    result = result,
    runtime = runtime,
    overhead = overhead,
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
  for (col in c("name", "command", "error", "traceback", "warnings")) {
    true(is.character(monad[[col]]))
  }
  for (col in c("runtime", "overhead")) {
    true(is.numeric(monad[[col]]))
  }
  true(!is.null(monad$result))
  invisible()
}

crew_monad_no_result <- structure(emptyenv(), class = "crew_monad_no_result")

#' @export
#' @keywords internal
print.crew_monad_no_result <- function(x, ...) {
  cat("<no result>")
}
