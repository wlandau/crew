monad_init <- function(
  name = NA_character_,
  command = NA_character_,
  result = NA,
  seconds = NA_real_,
  seed = NA_integer_,
  error = NA_character_,
  trace = NA_character_,
  warnings = NA_character_,
  launcher = NA_character_,
  worker = NA_integer_,
  instance = NA_character_
) {
  out <- monad_new(
    name = name,
    command = command,
    result = list(result),
    seconds = seconds,
    seed = seed,
    error = error,
    trace = trace,
    warnings = warnings,
    launcher = launcher,
    worker = worker,
    instance = instance
  )
  out
}

monad_new <- function(
  name = NULL,
  command = NULL,
  result = NULL,
  seconds = NULL,
  seed = NULL,
  error = NULL,
  trace = NULL,
  warnings = NULL,
  launcher = NULL,
  worker = NULL,
  instance = NULL
) {
  force(name)
  force(command)
  force(result)
  force(seconds)
  force(seed)
  force(error)
  force(trace)
  force(warnings)
  force(launcher)
  force(worker)
  force(instance)
  environment()
}

monad_validate <- function(monad) {
  crew_assert(is.environment(monad))
  crew_assert(identical(names(monad), names(formals(monad_new))))
  cols <- c(
    "name",
    "command",
    "error",
    "trace",
    "warnings",
    "launcher",
    "instance"
  )
  for (col in cols) {
    crew_assert(monad[[col]], is.character(.), length(.) == 1L)
  }
  for (col in c("seconds", "seed")) {
    crew_assert(monad[[col]], is.numeric(.), length(.) == 1L)
  }
  crew_assert(monad$worker, is.integer(.), length(.) == 1L)
  crew_assert(monad$result, is.list(.), length(.) == 1L)
  invisible()
}

monad_tibble <- function(monad) {
  out <- as.list(monad)
  attributes(out) <- list(
    names = monad_names,
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = .set_row_names(1L)
  )
  out
}

monad_names <- names(formals(monad_new))
monad_names_n <- length(monad_names)
