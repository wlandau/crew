monad_init <- function(
  name = NA_character_,
  command = NA_character_,
  result = NA,
  seconds = NA_real_,
  seed = NA_integer_,
  algorithm = NA_character_,
  status = NA_character_,
  code = NA_integer_,
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
    algorithm = algorithm,
    status = status,
    code = code,
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
  algorithm = NULL,
  status = NULL,
  code = NULL,
  error = NULL,
  trace = NULL,
  warnings = NULL,
  launcher = NULL,
  worker = NULL,
  instance = NULL
) {
  list(
    name = name,
    command = command,
    result = result,
    seconds = seconds,
    seed = seed,
    algorithm = algorithm,
    status = status,
    code = code,
    error = error,
    trace = trace,
    warnings = warnings,
    launcher = launcher,
    worker = worker,
    instance = instance
  )
}

monad_validate <- function(monad) {
  crew_assert(is.list(monad))
  crew_assert(identical(names(monad), names(formals(monad_new))))
  cols <- c(
    "name",
    "command",
    "algorithm",
    "status",
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
  for (col in c("code", "worker")) {
    crew_assert(monad[[col]], is.integer(.), length(.) == 1L)
  }
  crew_assert(monad$result, is.list(.), length(.) == 1L)
  invisible()
}

as_monad <- function(task, name) {
  out <- .subset2(task, "data")
  if (!is.list(out)) {
    out <- monad_init(
      name = name,
      status = if_any(
        identical(as.integer(out), 20L),
        "canceled",
        "error"
      ),
      code = as.integer(out),
      error = paste(
        utils::capture.output(print(out), type = "output"),
        collapse = "\n"
      )
    )
  }
  monad_tibble(out)
}

monad_tibble <- function(monad) {
  attributes(monad) <- list(
    names = monad_names,
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = monad_rownames
  )
  monad
}

monad_names <- names(formals(monad_new))
monad_names_n <- length(monad_names)
monad_rownames <- .set_row_names(1L)
