random_name <- function() {
  gsub(
    pattern = "-",
    replacement = "",
    x = uuid::UUIDgenerate(),
    fixed = TRUE
  )
}

is_named <- function(x) {
  if (!is.list(x) || length(x) < 1L) {
    return(TRUE)
  }
  n <- unique(names(x))
  !anyNA(n) && (length(n) == length(x)) && all(nzchar(n))
}
