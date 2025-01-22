monad_init <- function(
  name = NA_character_,
  command = NA_character_,
  result = NA,
  status = NA_character_,
  error = NA_character_,
  code = NA_integer_,
  trace = NA_character_,
  warnings = NA_character_,
  seconds = NA_real_,
  seed = NA_integer_,
  algorithm = NA_character_,
  launcher = NA_character_,
  worker = NA_character_
) {
  out <- monad_new(
    name = name,
    command = command,
    result = list(result),
    status = status,
    error = error,
    code = code,
    trace = trace,
    warnings = warnings,
    seconds = seconds,
    seed = seed,
    algorithm = algorithm,
    launcher = launcher,
    worker = worker
  )
  out
}

monad_new <- function(
  name = NULL,
  command = NULL,
  result = NULL,
  status = NULL,
  error = NULL,
  code = NULL,
  trace = NULL,
  warnings = NULL,
  seconds = NULL,
  seed = NULL,
  algorithm = NULL,
  launcher = NULL,
  worker = NULL
) {
  as_monad_tibble(
    list(
      name = name,
      command = command,
      result = result,
      status = status,
      error = error,
      code = code,
      trace = trace,
      warnings = warnings,
      seconds = seconds,
      seed = seed,
      algorithm = algorithm,
      launcher = launcher,
      worker = worker
    )
  )
}

monad_validate <- function(monad) {
  crew_assert(tibble::is_tibble(monad))
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
    "worker"
  )
  for (col in cols) {
    crew_assert(monad[[col]], is.character(.), length(.) == 1L)
  }
  for (col in c("seconds", "seed")) {
    crew_assert(monad[[col]], is.numeric(.), length(.) == 1L)
  }
  for (col in c("code")) {
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

as_monad_tibble <- function(object) {
  attributes(object) <- list(
    names = monad_names,
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = monad_rownames
  )
  object
}

monad_names <- names(formals(monad_new))
monad_names_n <- length(monad_names)
monad_rownames <- .set_row_names(1L)
