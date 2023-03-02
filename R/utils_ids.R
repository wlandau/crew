random_name <- function() {
  prefix <- basename(tempfile(pattern = ""))
  paste0(prefix, "_", format(Sys.time(), "%Y-%m-%d_%H-%M-%OS2"))
}
