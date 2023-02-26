#' @export
crew_monad <- function(command, envir = parent.frame()) {
  force(envir)
  command <- substitute(command)
  capture_error <- function(condition) {
    state$error <- monad_message(condition)
    state$error_class <- class(condition)
    state$traceback <- as.character(sys.calls())
    NULL
  }
  capture_warning <- function(condition) {
    state$count_warnings <- (state$count_warnings %||% 0L) + 1L
    should_store_warning <- (state$count_warnings < monad_max_warnings) &&
      (nchar(state$warnings %||% "") < monad_max_nchar)
    if (should_store_warning) {
      state$warnings <- paste(
        c(state$warnings, monad_message(condition)),
        collapse = ". "
      )
    }
    warning(as_immediate_condition(condition))
    invokeRestart("muffleWarning")
  }
  state <- new.env(hash = FALSE, parent = emptyenv())
  start <- as.numeric(proc.time()["elapsed"])
  result <- tryCatch(
    expr = withCallingHandlers(
      expr = eval(expr = command, envir = envir),
      error = capture_error,
      warning = capture_warnings
    ),
    error = function(condition) NULL
  )
  seconds <- as.numeric(proc.time()["elapsed"]) - start
  tibble::tibble(
    command = command,
    result = result,
    seconds = seconds,
    error = state$error,
    traceback = state$traceback,
    warnings = state$warnings
  )
}

monad_message <- function(condition, prefix = character(0)) {
  out <- monad_message_text_substring(
    message = conditionMessage(condition),
    prefix = prefix
  )
  if_any(nzchar(out), out, ".")
}

monad_message_text_substring <- function(message, prefix = character(0)) {
  tryCatch(
    substr(
      paste(c(prefix, message), collapse = " "),
      start = 0L,
      stop = monad_max_nchar
    ),
    error = function(condition) {
      paste(
        "crew could not process the error or warning message",
        "due to a text encoding issue."
      )
    }
  )
}

monad_max_nchar <- 2048L
monad_max_warnings <- 51L
