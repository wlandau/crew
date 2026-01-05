# Create a controller object from a client and launcher.

This function is for developers of `crew` launcher plugins. Users should
use a specific controller helper such as
[`crew_controller_local()`](https://wlandau.github.io/crew/reference/crew_controller_local.md).

## Usage

``` r
crew_controller(
  client,
  launcher,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE,
  crashes_max = 5L,
  backup = NULL,
  auto_scale = NULL
)
```

## Arguments

- client:

  An `R6` client object created by
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

- launcher:

  An `R6` launcher object created by one of the `crew_launcher_*()`
  functions such as
  [`crew_launcher_local()`](https://wlandau.github.io/crew/reference/crew_launcher_local.md).

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

- crashes_max:

  In rare cases, a worker may exit unexpectedly before it completes its
  current task. If this happens, `pop()` returns a status of `"crash"`
  instead of `"error"` for the task. The controller does not
  automatically retry the task, but you can retry it manually by calling
  `push()` again and using the same task name as before. (However,
  `targets` pipelines running `crew` do automatically retry tasks whose
  workers crashed.)

  `crashes_max` is a non-negative integer, and it sets the maximum
  number of allowable consecutive crashes for a given task. If a task's
  worker crashes more than `crashes_max` times in a row, then `pop()`
  throws an error when it tries to return the results of the task.

- backup:

  An optional `crew` controller object, or `NULL` to omit. If supplied,
  the `backup` controller runs any pushed tasks that have already
  reached `crashes_max` consecutive crashes. Using `backup`, you can
  create a chain of controllers with different levels of resources (such
  as worker memory and CPUs) so that a task that fails on one controller
  can retry using incrementally more powerful workers. All controllers
  in a backup chain should be part of the same controller group (see
  [`crew_controller_group()`](https://wlandau.github.io/crew/reference/crew_controller_group.md))
  so you can call the group-level `pop()` and `collect()` methods to
  make sure you get results regardless of which controller actually
  ended up running the task.

  Limitations of `backup`: \* `crashes_max` needs to be positive in
  order for `backup` to be used. Otherwise, every task would always skip
  the current controller and go to `backup`. \* `backup` cannot be a
  controller group. It must be an ordinary controller.

- auto_scale:

  Deprecated. Use the `scale` argument of `push()`, `pop()`, and
  `wait()` instead.

## See also

Other controller:
[`crew_class_controller`](https://wlandau.github.io/crew/reference/crew_class_controller.md)

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
client <- crew_client()
launcher <- crew_launcher_local()
controller <- crew_controller(client = client, launcher = launcher)
controller$start()
controller$push(name = "task", command = sqrt(4))
controller$wait()
controller$pop()
controller$terminate()
}
```
