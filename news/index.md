# Changelog

## crew 1.3.0.9002 (development)

- Use
  [`later::global_loop()`](https://later.r-lib.org/reference/create_loop.html)
  instead of
  [`later::current_loop()`](https://later.r-lib.org/reference/create_loop.html)
  in `controller$autoscale()`.
- Test
  [`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)
  with `autoexit = FALSE` (<https://github.com/r-lib/mirai/issues/535>).

## crew 1.3.0

CRAN release: 2025-09-13

- Reinstate checks that were temporarily suppressed to help plugins
  transition to [\#217](https://github.com/wlandau/crew/issues/217).
- Support `reset_globals`, `reset_packages`, `reset_options`, and
  `garbage_collection` in
  [`crew_controller_sequential()`](https://wlandau.github.io/crew/reference/crew_controller_sequential.md)
  ([\#217](https://github.com/wlandau/crew/issues/217)).
- Change argument `tls` to `tlscert` in the call to
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html)
  ([\#227](https://github.com/wlandau/crew/issues/227),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Use `collections` queues instead of custom queues
  ([\#229](https://github.com/wlandau/crew/issues/229)).
- Use `collections` dictionaries instead of hash environments to track
  tasks ([\#229](https://github.com/wlandau/crew/issues/229)).
- Remove the long-deprecated
  [`promise()`](https://rstudio.github.io/promises/reference/promise.html)
  method of the controller.
- Improve interoperability of `crew`’s workers by avoiding
  [`nanonext::cv_value()`](https://nanonext.r-lib.org/reference/cv.html)
  for condition variables
  ([\#225](https://github.com/wlandau/crew/issues/225)). Consequences:
  - `wait(mode = "all")` no longer guarantees that a task is available
    for `pop()`. It just consumes a condition variable signal. `pop()`
    should always be checked for `NULL` return values.
  - Until `mirai` gains a threaded dispatcher, `saturated()` needs to
    avoid the overhead of calling `status()` to get task counts. So the
    definition of “saturated” has changed: a controller is saturated if
    the number of *uncollected* tasks is greater than or equal to the
    maximum number of workers. Previously, it was the number of
    *unresolved* tasks.
  - Internal counters `.pushed` and `.popped` are safely removed.
  - Active bindings `pushed` and `popped` are removed.
- Controllers no longer have a `unpopped()` method because it is
  unnecessary to export and it assumes only the controller submits tasks
  to the compute profile (affecting interoperability).
- Allow a custom `later` loop in `autoscale()`.
- Deprecate the `pids()` methods in controllers and clients and stop
  babysitting the dispatcher process
  ([\#236](https://github.com/wlandau/crew/issues/236)).
- Deprecate
  [`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md)
  in favor of
  [`crew_monitor_local()`](https://wlandau.github.io/crew/reference/crew_monitor_local.md)
  ([\#236](https://github.com/wlandau/crew/issues/236)).
- Trim down the argument list of `launch_worker()` to just include
  arguments `call` and `name`.
- Simplify the auto-scaling algorithm to support greater
  interoperability and the eventual threaded dispatcher in `mirai`
  ([\#232](https://github.com/wlandau/crew/issues/232)).
- Deprecate `processes` and `async` in `crew` launchers in favor of job
  arrays ([\#218](https://github.com/wlandau/crew/issues/218),
  [\#237](https://github.com/wlandau/crew/issues/237)).
- Drop the unified controller group throttle. Let individual controllers
  handle their own throttling based on their own auto-scaling settings.
- When `tasks_mask` is finite, set `seconds_min` to `seconds_interval`
  in the throttle to give tasks enough time to accumulate.
- Use the counters in
  [`mirai::info()`](https://mirai.r-lib.org/reference/info.html) instead
  of `mirai::status()$events`
  ([\#232](https://github.com/wlandau/crew/issues/232),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Allow custom compute profiles in
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).
  This is mainly to support the use of the default compute profile to
  make `crew` more compatible with non-`crew` `mirai`-based workflows.
  It’s a quick solution for single controllers but does not work for
  controller groups.
- `crew_client()$start()` now errors if a `mirai` compute profile of the
  same name is already active.
- Shorten the length of strings from
  [`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md).

## crew 1.2.1

CRAN release: 2025-06-09

- CRAN patch (address MKL “Additional issues”).

## crew 1.2.0

CRAN release: 2025-06-06

- 22.7x speedup in `controller$walk()` with 100000 tasks: pre-compute
  the task list and defer auto-scaling until all tasks are pushed.
  Pushing a million tasks in `walk()` now takes just under 70 seconds on
  a local M2 Macbook.
- Use
  [`nanonext::ip_addr()`](https://nanonext.r-lib.org/reference/ip_addr.html)
  instead of `getip::getip()` to get the default IP address of the host
  ([\#216](https://github.com/wlandau/crew/issues/216),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Add a progress bar to `walk()` report progress when pushing tasks.
- Move arguments `reset_globals`, `reset_packages`, `reset_options`, and
  `garbage_collection` from the launcher to the controller
  ([\#217](https://github.com/wlandau/crew/issues/217)).
  [`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md)
  now handles cleanup directly, and workflows running `mirai` 2.0.0
  should mostly run faster because not all cleanup steps are
  automatically enabled.
- Avoid `mirai::call_mirai_()` in tests
  ([\#222](https://github.com/wlandau/crew/issues/222),
  [@shikokuchuo](https://github.com/shikokuchuo)).

## crew 1.1.2

CRAN release: 2025-05-08

- Speedup for `targets`: avoid superfluous processing in
  `controller$pop()`.
- Speedup queue methods, especially `queue$push()`, by making the queue
  non-portable and using direct access and direct assignment in `R6`.
- Reduce the frequency of calls to
  [`mirai::status()`](https://mirai.r-lib.org/reference/status.html) in
  workloads with a large number of quick tasks. Instead of resetting the
  throttle every time there is task or worker activity, only reset if
  there is worker activity. This was achieved by giving relay objects
  their own throttles instead of having them use the launcher throttles.
- Revert to a logical value for the `dispatcher` argument of
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
  ([\#215](https://github.com/wlandau/crew/issues/215),
  [@shikokuchuo](https://github.com/shikokuchuo)).

## crew 1.1.1

- Fix incorrect messages in documentation about deprecating the `name`
  and `workers` arguments of controllers. They are not deprecated at the
  controller level. They are only deprecated in
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md).

## crew 1.1.0

CRAN release: 2025-04-10

- Change prefix in `name_worker()` from `crew-` to `crew-worker-`.
- Use `call_mirai()` instead of the now deprecated `call_mirai_()`
  ([@shikokuchuo](https://github.com/shikokuchuo),
  [\#198](https://github.com/wlandau/crew/issues/198)).
- Add
  [`crew_controller_sequential()`](https://wlandau.github.io/crew/reference/crew_controller_sequential.md)
  ([\#205](https://github.com/wlandau/crew/issues/205)).
- Add a `serialization` argument to
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md)
  ([\#204](https://github.com/wlandau/crew/issues/204)).

## crew 1.0.0

CRAN release: 2025-02-03

- Avoid partial match in a call to
  [`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md).
- Migrate to `mirai` 2.0.0 with the pair 1 poly protocol
  ([\#193](https://github.com/wlandau/crew/issues/193)). Most notably,
  all workers now share the same URL instead of having different web
  sockets for different workers. This gets rid of `crew`’s previous
  “slotted” model for workers.
- Prohibit duplicated task names in `push()`. (The task must be popped
  first before another task of the same name can be submitted.) This
  allows controllers to use hash tables to track tasks, which makes
  `push()` and `pop()` orders of magnitude faster. It is also needed for
  the new retry mechanism based on backup controllers and controller
  groups.
- Use exponential backoff in throttles.
- Share throttles between [`scale()`](https://rdrr.io/r/base/scale.html)
  and `wait()` to increase responsiveness.
- Use a unified throttle for controller groups, which requires
  refactoring `wait(mode = "all")` for controller groups.
- Name every task and record every command.
- Switch URLs from `ws://` (and `wss://`) to `tcp://` (and `tls+tcp://`)
  ([\#193](https://github.com/wlandau/crew/issues/193)).
- Change R-level error status code to -1 (previously 1).
- Add a queue class for resolved tasks to make `pop()` and `collect()`
  more efficient. `pop()` is 64 times faster.
- For clarity, rename the `"launcher"` column of the results to
  `"controller"`.
- Log crashes and cancellations separately from ordinary errors in
  [`summary()`](https://rdrr.io/r/base/summary.html).

## crew 0.10.2

CRAN release: 2024-11-15

- Eliminate spurious `launch_max` error from underutilized workers
  ([\#189](https://github.com/wlandau/crew/issues/189)).
- Deprecate `launch_max` in favor of `crashes_error`
  ([\#189](https://github.com/wlandau/crew/issues/189)).
- Look for crashes of all workers in `rotate()` instead of looking for
  crashes of a specific worker in `launch()`
  ([\#189](https://github.com/wlandau/crew/issues/189)).
- Add a `crashes()` launcher method to allow plugins to detect and
  respond to crashes more easily.
- Change default `seconds_idle` to 300.
- Move `autometric` to `Suggests:`.

## crew 0.10.1

CRAN release: 2024-11-03

- Instrument
  [`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md)
  with
  [`autometric::log_phase_set()`](https://wlandau.github.io/autometric/reference/log_phase_set.html)
  and
  [`autometric::log_phase_reset()`](https://wlandau.github.io/autometric/reference/log_phase_reset.html).

## crew 0.10.0

CRAN release: 2024-10-11

- Give full worker name in the worker launch error message
  ([@multimeric](https://github.com/multimeric)).
- Start the controller automatically in more methods.
- Add an `r_arguments` argument to supply command line arguments to R
  for workers ([\#175](https://github.com/wlandau/crew/issues/175),
  [@rpruim](https://github.com/rpruim)).
- Add a `cancel()` controller method to cancel one or more tasks.
- Call `cancel(all = TRUE)` from `terminate()` as a safeguard.
- Make `client$dispatcher` a
  [`ps::ps_handle()`](https://ps.r-lib.org/reference/ps_handle.html)
  handle and add a new `client$client` handle for the current process.
- To passively log memory usage when `log_resources` is given, the
  controller now calls [`log()`](https://rdrr.io/r/base/Log.html) as a
  side effect in most controller methods, with throttling to preserve
  speed.
- Return a status and status code from `pop()` etc.
- New internal function `as_monad()` makes error reporting more
  consistent.
- Use [`path.expand()`](https://rdrr.io/r/base/path.expand.html) on
  local log files.
- Switch to `Rscript`.
- Print session info from
  [`crew_worker()`](https://wlandau.github.io/crew/reference/crew_worker.md)
  before attempting to dial into `mirai`.
- Add `asyncdial = FALSE` back to
  [`mirai::daemon()`](https://mirai.r-lib.org/reference/daemon.html)
  call.
- Deprecate `local_log_directory` and `local_log_join` in favor of
  [`crew_options_local()`](https://wlandau.github.io/crew/reference/crew_options_local.md)
  and the `options_local` argument.
- Add
  [`crew_options_metrics()`](https://wlandau.github.io/crew/reference/crew_options_metrics.md)
  and the `options_metrics` argument for recording resource metrics
  ([\#178](https://github.com/wlandau/crew/issues/178)).
- Add a new `logging.Rmd` vignette to explain best practices for logging
  and resource usage metrics
  ([\#178](https://github.com/wlandau/crew/issues/178)).

## crew 0.9.5

CRAN release: 2024-06-24

- CRAN patch.

## crew 0.9.4

CRAN release: 2024-06-20

- Do not use extended tasks in Shiny vignette.
- Add a new `retry_tasks` argument with default `TRUE`
  ([\#170](https://github.com/wlandau/crew/issues/170)).
- Avoid Base64 encoding functions from `nanonext`

## crew 0.9.3

CRAN release: 2024-05-22

- Deprecate native `crew` promises in favor of native event-driven
  promises in `mirai`
  ([\#162](https://github.com/wlandau/crew/issues/162)).
- Update the `shiny.Rmd` and `promises.Rmd` vignette to recommend native
  event-driven `mirai` promises
  ([\#162](https://github.com/wlandau/crew/issues/162)).

## crew 0.9.2

CRAN release: 2024-04-24

- Use `.args` rather than `...` in
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) to
  make sure arguments continue to be passed as local variables in
  `mirai` \>= 0.13.1.9012.
- Add new controller methods `autoscale()`, `descale()`, and `started()`
  to facilitate different kinds of Shiny apps.
- Deprecate the `scale` and `throttle` methods of
  `controller$promise()`.
  [`promise()`](https://rstudio.github.io/promises/reference/promise.html)
  now always calls `autoscale()` to make sure one and only one
  auto-scaling loop is running asynchronously. Auto-scaling thus
  continues even after the promise resolves.
- Add a second example vignette that simulates coin flips.
- Add a new `error` argument to `collect()`
  ([\#166](https://github.com/wlandau/crew/issues/166)).

## crew 0.9.1

CRAN release: 2024-03-25

- Rewrite the async Shiny vignette with `crew` promises and Shiny
  extended tasks ([\#157](https://github.com/wlandau/crew/issues/157),
  [@jcheng5](https://github.com/jcheng5)).
- Clarify the intent of `controller$promise(mode = "one")` in the
  vignette on promises ([@jcheng5](https://github.com/jcheng5)).
- Implement an `error` argument in `pop()` which may help with
  integration with `ExtendedTask`
  ([@jcheng5](https://github.com/jcheng5)).
- Handle task errors in the Shiny vignette
  ([@jcheng5](https://github.com/jcheng5)).

## crew 0.9.0

CRAN release: 2024-02-08

- Require `nanonext` \>= 0.12.0 and `mirai` \>= 0.12.0.
- Return to always re-launching backlogged inactive workers
  ([\#79](https://github.com/wlandau/crew/issues/79),
  <https://github.com/r-lib/mirai/discussions/95>).
- Implement `push_backlog()` and `pop_backlog()` to manage cases when it
  is not desirable to push to saturated controllers
  (<https://github.com/ropensci/targets/issues/1220>).
- Invisibly return the `mirai` object of a task pushed to the
  controller. This allows users to interact with the task directly,
  e.g. to create a promise object with
  [`promises::as.promise()`](https://rstudio.github.io/promises/reference/is.promise.html)
  ([\#146](https://github.com/wlandau/crew/issues/146),
  [@jcheng5](https://github.com/jcheng5)).
- Add a new `walk()` method for controllers and controller groups to
  submit tasks in batch and return control immediately without waiting
  for any task to complete
  ([\#148](https://github.com/wlandau/crew/issues/148),
  [@jcheng5](https://github.com/jcheng5)).
- Revive the `collect()` method for popping multiple tasks at once
  ([\#148](https://github.com/wlandau/crew/issues/148),
  [@jcheng5](https://github.com/jcheng5)).
- Add controller group methods `nonempty()`, `resolved()`,
  `unresolved()`, and `unpopped()` to help with
  [\#148](https://github.com/wlandau/crew/issues/148).
- Make the `mirai` dispatcher error message extremely verbose.

## crew 0.8.0

CRAN release: 2024-01-09

- Configure workers to send themselves a termination signal if the
  connection to the dispatcher is broken
  ([\#141](https://github.com/wlandau/crew/issues/141),
  [@psychelzh](https://github.com/psychelzh)). Huge thanks to
  [@shikokuchuo](https://github.com/shikokuchuo) for the support through
  <https://github.com/r-lib/mirai/issues/87>,
  <https://github.com/r-lib/mirai/pull/88>, and
  <https://github.com/r-lib/nanonext/pull/25>! The signal itself is
  platform-dependent and determined by the new function
  [`crew_terminate_signal()`](https://wlandau.github.io/crew/reference/crew_terminate_signal.md).
- Implement
  [`crew_monitor_local()`](https://wlandau.github.io/crew/reference/crew_monitor_local.md)
  to help users monitor and terminate local R processes created by
  `crew` and `mirai`.
- Implement new utility function
  [`crew_terminate_process()`](https://wlandau.github.io/crew/reference/crew_terminate_process.md)
  to terminate a process manually without resorting to `SIGKILL` on
  Windows.
- Throw a warning from `controller$map()` if at least one task threw
  one. `warnings = FALSE` suppresses this behavior.
- Set `output = TRUE` in `daemon()` so `stdout` and `stderr` streams
  print.
- Add new arguments `local_log_directory` and `local_log_join` to write
  to local log files.

## crew 0.7.0

CRAN release: 2023-12-11

- Migrate from `asyncdial` to `autoexit`.
- Use `Sys.info()[["user"]]` to get the user in
  [`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md)
  ([\#135](https://github.com/wlandau/crew/issues/135),
  [@luwidmer](https://github.com/luwidmer)).
- Use condition variables to wait for tasks more efficiently
  ([\#108](https://github.com/wlandau/crew/issues/108)).
- Because of [\#108](https://github.com/wlandau/crew/issues/108),
  `controller$map()` can no longer be used if there are tasks waiting to
  be popped.
- Use a `cli` progress bar in `map()`.
- Encapsulate non-function `R6` members in the `private` list and add
  active bindings where interfaces are necessary
  ([\#137](https://github.com/wlandau/crew/issues/137)). Ad hoc tests
  that absolutely need to modify private objects can use
  `object$.__enclos_env__$private` trick, but these tests should be
  skipped on CRAN in case there is a change to `R6` that breaks this.
- Drop `.signal` from
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html) since
  all `mirai` tasks signal as of `mirai` version 0.11.2.9025.
- Implement
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md),
  a decoupled mechanism for throttling that can be applied to scaling.
- Bring back the `throttle` argument, powered by
  [`crew_throttle()`](https://wlandau.github.io/crew/reference/crew_throttle.md).
- Retry
  [`mirai::status()`](https://mirai.r-lib.org/reference/status.html)
  again in `daemons_info()` and make it configurable using
  `seconds_interval` and `seconds_timeout` in both the client and the
  launcher ([\#128](https://github.com/wlandau/crew/issues/128)).

## crew 0.6.0

CRAN release: 2023-10-12

- Migrate checks to enforce features in version 0.5.0 for reverse
  dependencies.
- Drop check of backlogged workers, c.f.
  <https://github.com/r-lib/mirai/discussions/63#discussioncomment-7051889>
  ([\#79](https://github.com/wlandau/crew/issues/79),
  [\#124](https://github.com/wlandau/crew/issues/124),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Deprecate `seconds_exit` because `exitlinger` in `mirai` is now
  obsolete ([\#125](https://github.com/wlandau/crew/issues/125),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Use `mirai::nextget("cv")` to count unresolved tasks instead of
  looping through all the task objects
  ([\#131](https://github.com/wlandau/crew/issues/131)).
- Remove throttling and `collect()` in auto-scaling. Simplifies much of
  the code. Made possible by the efficiency gains in
  [\#131](https://github.com/wlandau/crew/issues/131).
- Simplify `wait()`.
- `seconds_interval` in `map()` no longer defaults to
  `controller$client$seconds_interval`.
- `launcher$terminate_workers()` terminates one or more workers, and
  `launcher$terminate` terminates the whole launcher.
- Add infrastructure to let custom launcher plugins launch and terminate
  workers asynchronously
  ([\#133](https://github.com/wlandau/crew/issues/133)). Launchers can
  set a positive number in the `processes` field to set the number of
  local `mirai` daemons for asynchronous requests to launch and
  terminate the serious workers. Then, `launch_worker()` and
  `terminate_worker()` can optionally make use of
  `launcher$async$eval()` to send these asynchronous calls.

## crew 0.5.0

CRAN release: 2023-09-18

- Suppress interactive browser on Windows which launched on each worker
  previously ([@psychelzh](https://github.com/psychelzh)).
- Migrate to the new host/daemon nomenclature in `mirai` 0.9.1
  ([\#96](https://github.com/wlandau/crew/issues/96)).
- Suppress `status()` retries
  ([@shikokuchuo](https://github.com/shikokuchuo),
  [\#100](https://github.com/wlandau/crew/issues/100)).
- Implement `launch_max` to error out if workers repeatedly launch
  without completing any tasks
  ([\#101](https://github.com/wlandau/crew/issues/101),
  [@shikokuchuo](https://github.com/shikokuchuo),
  [@multimeric](https://github.com/multimeric)).
- Detect discovered workers more robustly in `launcher$done()`.
- Add a new `algorithm` argument to the `push()`, `shove()`, and `map()`
  methods of controllers and controller groups, as well as
  arguments/fields in
  [`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md)
  and its return value
  ([\#113](https://github.com/wlandau/crew/issues/113),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- As a default for pseudo-random number generation, leverage
  widely-spaced L’Ecuyer streams as supported by
  [`mirai::nextstream()`](https://mirai.r-lib.org/reference/nextstream.html)
  ([\#115](https://github.com/wlandau/crew/issues/115),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Move README documentation to vignettes.
- Add a new
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md)
  function for TLS configuration
  ([\#120](https://github.com/wlandau/crew/issues/120)).
- Deprecate the `tls_enable` and `tls_config` arguments of
  [`crew_client()`](https://wlandau.github.io/crew/reference/crew_client.md)
  etc. in favor of a `tls` argument which accepts
  [`crew_tls()`](https://wlandau.github.io/crew/reference/crew_tls.md)
  objects ([\#120](https://github.com/wlandau/crew/issues/120)).

## crew 0.4.0

CRAN release: 2023-07-10

### Highlights

- Use the TLS capabilities of `nanonext` and `mirai`.
- Add a `map()` method to the controller class for functional
  programming ([\#93](https://github.com/wlandau/crew/issues/93)).
- Fix bug in [`scale()`](https://rdrr.io/r/base/scale.html) which
  launched too many workers when `active - demand` was less than 0.
- Add
  [`crew_clean()`](https://wlandau.github.io/crew/reference/crew_clean.md)
  to help clean up any lingering dispatchers and workers from previous
  sessions.

### Other improvements

- Remove temporary controller summary workaround that ensured CRAN
  compatibility with `targets` 1.1.3.
- Add guardrails to make sure the controller is started for methods that
  require this. Part of this involved improving the error messages.
- Retry `daemons()` in `daemons_info()`.

## crew 0.3.0

CRAN release: 2023-06-26

- Track warnings properly
  ([\#81](https://github.com/wlandau/crew/issues/81),
  [@brendanf](https://github.com/brendanf)).
- Optimize heavily ([\#81](https://github.com/wlandau/crew/issues/81),
  [\#83](https://github.com/wlandau/crew/issues/83),
  [@shikokuchuo](https://github.com/shikokuchuo),
  [@brendanf](https://github.com/brendanf)).
- Use CRAN `nanonext` 0.9.0.
- Delegate the task pushing and collection logic to a new `R6` schedule
  class ([\#84](https://github.com/wlandau/crew/issues/84)).
- Delegate auto-scaling to the launcher
  ([\#84](https://github.com/wlandau/crew/issues/84)).
- Drastically simplify internal logic
  ([\#84](https://github.com/wlandau/crew/issues/84)).
- Rename “router” to “client” and change the function signature of
  `launch_worker()` ([\#84](https://github.com/wlandau/crew/issues/84)).
  Unfortunately neither of these changes is back-compatible for
  third-party launcher plugins. However, the controller interface is
  still back-compatible, so `targets` remains unaffected.
- Terminate “lost” workers on websocket rotation.
- Allow `NULL` result in `pop()$result[[1L]]`.
- Delegate summaries among the controller, schedule, launcher, and
  client.
- Default to the loopback address for the local controller.
- Use already cumulative `assigned` and `complete` stats from `mirai`
  0.8.7.9029.
- Reorganize `pkgdown` reference and `Rd` families to clarify which
  parts are for end-users and which parts are for launcher plugin
  developers.
- Add `controller$shove()` and `schedule$list()` to support fast
  extensions for functional programming.
- Depend on `mirai` 0.9.0 or greater.

## crew 0.2.1

CRAN release: 2023-05-22

- Use [`packageStartupMessage()`](https://rdrr.io/r/base/message.html).

## crew 0.2.0

- Use `.args` instead of `...` in
  [`mirai::mirai()`](https://mirai.r-lib.org/reference/mirai.html).
- Use [`set.seed()`](https://rdrr.io/r/base/Random.html) instead of
  [`withr::local_seed()`](https://withr.r-lib.org/reference/with_seed.html)
  ([\#67](https://github.com/wlandau/crew/issues/67),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Add a vignette to showcase integration with Shiny, co-authored with
  [@dwoodie](https://github.com/dwoodie) and with helpful suggestions by
  [@shikokuchuo](https://github.com/shikokuchuo).
- Drop
  [`rlang::duplicate()`](https://rlang.r-lib.org/reference/duplicate.html)
  ([\#72](https://github.com/wlandau/crew/issues/72),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Do not poll dispatcher availability when starting router objects
  ([@shikokuchuo](https://github.com/shikokuchuo)).
- Implement a `saturated()` controller method to support `targets`.
- Add a `worker_index` column to controller summaries.
- When relaunching workers, prioritize those with unresolved tasks stuck
  at the NNG level ([\#75](https://github.com/wlandau/crew/issues/75),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Always relaunch backlogged inactive workers
  ([\#79](https://github.com/wlandau/crew/issues/79)).
- Deprecate the `auto_scale` argument/field of controllers in favor of
  the `scale` argument of `push()`, `pop()`, and `wait()`.
- Throttle auto-scaling with interval `self$router$seconds_interval`
  ([\#76](https://github.com/wlandau/crew/issues/76)).
- Remove `clean()` and instead terminate lost workers on launch.
- Fix examples.
- Add a `collect` argument to `pop()`.

## crew 0.1.1

CRAN release: 2023-04-21

- Ensure checks pass on CRAN.

## crew 0.1.0

CRAN release: 2023-04-19

- Remove `asyncdial` from call to
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html).
- Explain vision in the launcher plugin vignette.
- Remove the superfluous `clean()` controller method.
- Clean up logic for [`scale()`](https://rdrr.io/r/base/scale.html) and
  `launch()` controller methods.
- Add a new `inactive()` launcher method to help with the above.
- Eliminate superfluous worker launches: clean up the controller logic,
  and check worker discoveries before worker connections
  ([\#51](https://github.com/wlandau/crew/issues/51)).
- Simplify the launcher plugin interface
  ([\#54](https://github.com/wlandau/crew/issues/54)).
- Commit to using the fast
  [`nanonext::.unresolved()`](https://nanonext.r-lib.org/reference/dot-unresolved.html)
  ([\#49](https://github.com/wlandau/crew/issues/49)).
- Rename the `callr` launcher to the local process launcher
  ([\#56](https://github.com/wlandau/crew/issues/56)).
- Switch from SHA224 to SHA1 as the hash for random tokens and names
  ([\#59](https://github.com/wlandau/crew/issues/59),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Open and confirm bus socket listeners asynchronously in the launcher
  abstract class ([\#58](https://github.com/wlandau/crew/issues/58)).
- Use condition variables to test the connection status of worker
  instances ([\#57](https://github.com/wlandau/crew/issues/57),
  [@shikokuchuo](https://github.com/shikokuchuo)).
- Allow the dispatcher process to not exist so that the router will keep
  working if `mirai` implements the dispatcher another way
  ([\#60](https://github.com/wlandau/crew/issues/60)).
- Add a `substitute` argument to `push()`
  ([\#63](https://github.com/wlandau/crew/issues/63)).
- Remove dependency on custom NNG bus sockets, which significantly
  simplifies the internals, improves robustness, and smooths the
  integration with `mirai`
  ([\#61](https://github.com/wlandau/crew/issues/61)).
- Use `processx` instead of `callr` for the local process launcher
  ([\#64](https://github.com/wlandau/crew/issues/64)).
- Speed up
  [`crew_eval()`](https://wlandau.github.io/crew/reference/crew_eval.md)
  by e.g. relying on
  [`nanonext::mclock()`](https://nanonext.r-lib.org/reference/mclock.html)
  for the time.
- Add an `empty()` method to check if the controller or controller group
  is empty.
- Rely on `mirai` for globals cleanup, garbage collection, and global
  option resets ([\#65](https://github.com/wlandau/crew/issues/65)).
- Expose the package unloading capabilities of `mirai`
  ([\#65](https://github.com/wlandau/crew/issues/65)).
- Make monads classed environments and leave tibbles to the last minute
  in `pop()`.
- Rename `crew_wait()` to
  [`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md).
- Export
  [`crew_assert()`](https://wlandau.github.io/crew/reference/crew_assert.md)
  and
  [`crew_random_name()`](https://wlandau.github.io/crew/reference/crew_random_name.md)
  to help launcher plugins.
- Rename the `listen()` method to
  [`start()`](https://rdrr.io/r/stats/start.html) in the router class.
- The `poll()` method now retries
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html)
  for robustness.
- [`crew_retry()`](https://wlandau.github.io/crew/reference/crew_retry.md)
  gains a `max_tries` argument.
- Bug fix: stop setting `router$daemons` to `NULL` in `poll()` when the
  connection has a momentary problem.
- Simplify `listening()`.

## crew 0.0.5

CRAN release: 2023-03-23

- Write a [`call()`](https://rdrr.io/r/base/call.html) method to make it
  easier to write custom launcher plugins.
- Add robust documentation and tests for custom launcher plugins.
- Use [`as.character()`](https://rdrr.io/r/base/character.html) to make
  sure the `list` argument supplied to
  [`rm()`](https://rdrr.io/r/base/rm.html) is safe. Fixes a test that
  was failing on R-devel with `crew` version 0.0.4.
- Remove the `seconds_poll_high` and `seconds_poll_low` fields and
  arguments for compatibility with `mirai` development and release
  versions.
- Hard-code the `async_dial` field in the router for compatibility with
  `mirai` development and release versions.

## crew 0.0.4

CRAN release: 2023-03-21

- Adjust references and tests as requested by CRAN.

## crew 0.0.3

- First version.
