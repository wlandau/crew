# Create a controller group.

Create an `R6` object to submit tasks and launch workers through
multiple `crew` controllers.

## Usage

``` r
crew_controller_group(..., seconds_interval = 0.25)
```

## Arguments

- ...:

  `R6` controller objects or lists of `R6` controller objects. Nested
  lists are allowed, but each element must be a control object or
  another list.

- seconds_interval:

  Number of seconds between polling intervals waiting for certain
  internal synchronous operations to complete, such as checking
  [`mirai::info()`](https://mirai.r-lib.org/reference/info.html)

## See also

Other controller_group:
[`crew_class_controller_group`](https://wlandau.github.io/crew/reference/crew_class_controller_group.md)

## Examples

``` r
if (identical(Sys.getenv("CREW_EXAMPLES"), "true")) {
persistent <- crew_controller_local(name = "persistent")
transient <- crew_controller_local(
  name = "transient",
  tasks_max = 1L
)
group <- crew_controller_group(persistent, transient)
group$start()
group$push(name = "task", command = sqrt(4), controller = "transient")
group$wait()
group$pop()
group$terminate()
}
```
