name_worker <- function(launcher, worker, instance) {
  paste("crew", launcher, worker, instance, sep = "-")
}

parse_instance <- function(socket) {
  split <- strsplit(socket, split = "/", fixed = TRUE)[[1]]
  split[length(split)]
}

is_named <- function(x) {
  if (!is.list(x) || length(x) < 1L) {
    return(TRUE)
  }
  n <- unique(names(x))
  !anyNA(n) && (length(n) == length(x)) && all(nzchar(n))
}

eval_tidyselect <- function(expr, choices) {
  if (is.null(rlang::quo_squash(expr)) || !length(choices)) {
    return(character(0L))
  }
  choices <- unique(choices)
  names(choices) <- choices
  out <- tidyselect::eval_select(
    expr = expr,
    data = choices,
    strict = FALSE
  )
  out <- names(out)
  crew_assert(
    is.character(out),
    message = paste(
      "tidyselect failed. Please supply a valid tidyselect expression",
      "such as everything() or any_of(\"this_field\")."
    )
  )
  out
}
