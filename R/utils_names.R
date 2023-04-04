random_name <- function() {
  nanonext::sha1(nanonext::random(n = 100L))
}

parse_socket <- function(socket) {
  path <- nanonext::parse_url(socket)["path"]
  path <- strsplit(path, split = "/", fixed = TRUE)[[1]]
  path <- path[nzchar(path)]
  if_any(
    length(path) < 2L,
    list(index = 1L, instance = path),
    list(index = path[1L], instance = path[2L])
  )
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
