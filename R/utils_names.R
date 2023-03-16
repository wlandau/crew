random_name <- function() {
  nanonext::sha224(nanonext::random(n = 100))
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
  true(
    is.character(out),
    message = paste(
      "tidyselect failed. Please supply a valid tidyselect expression",
      "such as everything() or any_of(\"this_field\")."
    )
  )
  out
}
