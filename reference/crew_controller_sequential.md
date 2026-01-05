# Create a sequential controller.

The sequential controller runs tasks on the same R process where the
controller object exists. Tasks run sequentially rather than in
parallel.

## Usage

``` r
crew_controller_sequential(
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE
)
```

## Arguments

- reset_globals:

  `TRUE` to reset global environment variables between tasks, `FALSE` to
  leave them alone.

- reset_packages:

  `TRUE` to detach any packages loaded during a task (runs between each
  task), `FALSE` to leave packages alone. In either case, the namespaces
  are not detached.

- reset_options:

  `TRUE` to reset global options to their original state between each
  task, `FALSE` otherwise. It is recommended to only set
  `reset_options = TRUE` if `reset_packages` is also `TRUE` because
  packages sometimes rely on options they set at loading time. for this
  and other reasons, `reset_options` only resets options that were
  nonempty at the beginning of the task. If your task sets an entirely
  new option not already in
  [`options()`](https://rdrr.io/r/base/options.html), then
  `reset_options = TRUE` does not delete the option.

- garbage_collection:

  `TRUE` to run garbage collection after each task task, `FALSE` to
  skip.

## See also

Other sequential controllers:
[`crew_class_controller_sequential`](https://wlandau.github.io/crew/reference/crew_class_controller_sequential.md)

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
controller <- crew_controller_sequential()
controller$push(name = "task", command = sqrt(4))
controller$pop()
}
```
