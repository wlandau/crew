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
  as_class(environment(), "crew_monad")
}

monad_validate <- function(monad) {
  crew_assert(inherits(monad, "crew_monad"))
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

#' @export
print.crew_monad <- function(x, ...) {
  cat("<crew_monad>\n ", paste0(paste_list(as.list(x)), collapse = "\n  "))
}
