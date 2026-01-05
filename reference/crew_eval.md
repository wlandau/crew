# Evaluate an R command and return results as a monad.

Not a user-side function. Do not call directly.

## Usage

``` r
crew_eval(
  command,
  name,
  data = list(),
  globals = list(),
  seed = NULL,
  algorithm = NULL,
  packages = character(0),
  library = NULL,
  reset_globals = TRUE,
  reset_packages = FALSE,
  reset_options = FALSE,
  garbage_collection = FALSE
)
```

## Arguments

- command:

  Language object with R code to run.

- name:

  Character of length 1, name of the task.

- data:

  Named list of local data objects in the evaluation environment.

- globals:

  Named list of objects to temporarily assign to the global environment
  for the task.

- seed:

  Integer of length 1 with the pseudo-random number generator seed to
  set for the evaluation of the task. Passed to the `seed` argument of
  [`set.seed()`](https://rdrr.io/r/base/Random.html) if not `NULL`. If
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the recommended widely spaced worker-specific
  L'Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- algorithm:

  Integer of length 1 with the pseudo-random number generator algorithm
  to set for the evaluation of the task. Passed to the `kind` argument
  of [`RNGkind()`](https://rdrr.io/r/base/Random.html) if not `NULL`. If
  `algorithm` and `seed` are both `NULL`, then the random number
  generator defaults to the recommended widely spaced worker-specific
  L'Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html).
  See
  [`vignette("parallel", package = "parallel")`](https://cran.rstudio.com/web/packages/parallel/vignettes/parallel.pdf)
  for details.

- packages:

  Character vector of packages to load for the task.

- library:

  Library path to load the packages. See the `lib.loc` argument of
  [`require()`](https://rdrr.io/r/base/library.html).

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

## Value

A monad object with results and metadata.

## Details

The `crew_eval()` function evaluates an R expression in an encapsulated
environment and returns a monad with the results, including warnings and
error messages if applicable. The random number generator seed,
`globals`, and global options are restored to their original values on
exit.

## See also

Other utility:
[`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md),
[`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md),
[`crew_deprecate()`](https://wlandau.github.io/crew/reference/crew_deprecate.md),
[`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md),
[`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md),
[`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md),
[`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md),
[`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)

## Examples

``` r
crew_eval(quote(1 + 1), name = "task_name")
#> # A tibble: 1 × 12
#>   name  command result status error  code trace warnings seconds  seed algorithm
#>   <chr> <chr>   <list> <chr>  <chr> <int> <chr> <chr>      <dbl> <int> <chr>    
#> 1 task… 1 + 1   <dbl>  succe… NA        0 NA    NA             0    NA NA       
#> # ℹ 1 more variable: controller <chr>
```
