# crew 1.3.0

* Reinstate checks that were temporarily suppressed to help plugins transition to #217.
* Support `reset_globals`, `reset_packages`, `reset_options`, and `garbage_collection` in `crew_controller_sequential()` (#217).
* Change argument `tls` to `tlscert` in the call to `mirai::daemon()` (#227, @shikokuchuo).
* Use `collections` queues instead of custom queues (#229).
* Use `collections` dictionaries instead of hash environments to track tasks (#229).
* Remove the long-deprecated `promise()` method of the controller.
* Improve interoperability of `crew`'s workers by avoiding `nanonext::cv_value()` for condition variables (#225). Consequences:
    * `wait(mode = "all")` no longer guarantees that a task is available for `pop()`. It just consumes a condition variable signal. `pop()` should always be checked for `NULL` return values.
    * Until `mirai` gains a threaded dispatcher, `saturated()` needs to avoid the overhead of calling `status()` to get task counts. So the definition of "saturated" has changed: a controller is saturated if the number of *uncollected* tasks is greater than or equal to the maximum number of workers. Previously, it was the number of *unresolved* tasks.
    * Internal counters `.pushed` and `.popped` are safely removed.
    * Active bindings `pushed` and `popped` are removed.
* Controllers no longer have a `unpopped()` method because it is unnecessary to export and it assumes only the controller submits tasks to the compute profile (affecting interoperability).
* Allow a custom `later` loop in `autoscale()`.
* Deprecate the `pids()` methods in controllers and clients and stop babysitting the dispatcher process (#236).
* Deprecate `crew_clean()` in favor of `crew_monitor_local()` (#236).
* Trim down the argument list of `launch_worker()` to just include arguments `call` and `name`.
* Simplify the auto-scaling algorithm to support greater interoperability and the eventual threaded dispatcher in `mirai` (#232).
* Deprecate `processes` and `async` in `crew` launchers in favor of job arrays (#218, #237).
* Drop the unified controller group throttle. Let individual controllers handle their own throttling based on their own auto-scaling settings.
* When `tasks_mask` is finite, set `seconds_min` to `seconds_interval` in the throttle to give tasks enough time to accumulate.
* Use the counters in `mirai::info()` instead of `mirai::status()$events` (#232, @shikokuchuo).
* Allow custom compute profiles in `crew_client()`. This is mainly to support the use of the default compute profile to make `crew` more compatible with non-`crew` `mirai`-based workflows. It's a quick solution for single controllers but does not work for controller groups.
* `crew_client()$start()` now errors if a `mirai` compute profile of the same name is already active.
* Shorten the length of strings from `crew_random_name()`.

# crew 1.2.1

* CRAN patch (address MKL "Additional issues").

# crew 1.2.0

* 22.7x speedup in `controller$walk()` with 100000 tasks: pre-compute the task list and defer auto-scaling until all tasks are pushed. Pushing a million tasks in `walk()` now takes just under 70 seconds on a local M2 Macbook.
* Use `nanonext::ip_addr()` instead of `getip::getip()` to get the default IP address of the host (#216, @shikokuchuo).
* Add a progress bar to `walk()` report progress when pushing tasks.
* Move arguments `reset_globals`, `reset_packages`, `reset_options`, and `garbage_collection` from the launcher to the controller (#217). `crew_eval()` now handles cleanup directly, and workflows running `mirai` 2.0.0 should mostly run faster because not all cleanup steps are automatically enabled.
* Avoid `mirai::call_mirai_()` in tests (#222, @shikokuchuo).

# crew 1.1.2

* Speedup for `targets`: avoid superfluous processing in `controller$pop()`.
* Speedup queue methods, especially `queue$push()`, by making the queue non-portable and using direct access and direct assignment in `R6`.
* Reduce the frequency of calls to `mirai::status()` in workloads with a large number of quick tasks. Instead of resetting the throttle every time there is task or worker activity, only reset if there is worker activity. This was achieved by giving relay objects their own throttles instead of having them use the launcher throttles.
* Revert to a logical value for the `dispatcher` argument of `mirai::daemons()` (#215, @shikokuchuo).

# crew 1.1.1

* Fix incorrect messages in documentation about deprecating the `name` and `workers` arguments of controllers. They are not deprecated at the controller level. They are only deprecated in `crew_client()`.

# crew 1.1.0

* Change prefix in `name_worker()` from `crew-` to `crew-worker-`.
* Use `call_mirai()` instead of the now deprecated `call_mirai_()` (@shikokuchuo, #198).
* Add `crew_controller_sequential()` (#205).
* Add a `serialization` argument to `crew_client()` (#204).

# crew 1.0.0

* Avoid partial match in a call to `crew_terminate_process()`.
* Migrate to `mirai` 2.0.0 with the pair 1 poly protocol (#193). Most notably, all workers now share the same URL instead of having different web sockets for different workers. This gets rid of `crew`'s previous "slotted" model for workers.
* Prohibit duplicated task names in `push()`. (The task must be popped first before another task of the same name can be submitted.) This allows controllers to use hash tables to track tasks, which makes `push()` and `pop()` orders of magnitude faster. It is also needed for the new retry mechanism based on backup controllers and controller groups.
* Use exponential backoff in throttles.
* Share throttles between `scale()` and `wait()` to increase responsiveness.
* Use a unified throttle for controller groups, which requires refactoring `wait(mode = "all")` for controller groups.
* Name every task and record every command.
* Switch URLs from `ws://` (and `wss://`) to `tcp://` (and `tls+tcp://`) (#193).
* Change R-level error status code to -1 (previously 1).
* Add a queue class for resolved tasks to make `pop()` and `collect()` more efficient. `pop()` is 64 times faster.
* For clarity, rename the `"launcher"` column of the results to `"controller"`.
* Log crashes and cancellations separately from ordinary errors in `summary()`.

# crew 0.10.2

* Eliminate spurious `launch_max` error from underutilized workers (#189).
* Deprecate `launch_max` in favor of `crashes_error` (#189).
* Look for crashes of all workers in `rotate()` instead of looking for crashes of a specific worker in `launch()` (#189).
* Add a `crashes()` launcher method to allow plugins to detect and respond to crashes more easily.
* Change default `seconds_idle` to 300.
* Move `autometric` to `Suggests:`.

# crew 0.10.1

* Instrument `crew_eval()` with `autometric::log_phase_set()` and `autometric::log_phase_reset()`.

# crew 0.10.0

* Give full worker name in the worker launch error message (@multimeric).
* Start the controller automatically in more methods.
* Add an `r_arguments` argument to supply command line arguments to R for workers (#175, @rpruim).
* Add a `cancel()` controller method to cancel one or more tasks.
* Call `cancel(all = TRUE)` from `terminate()` as a safeguard.
* Make `client$dispatcher` a `ps::ps_handle()` handle and add a new `client$client` handle for the current process.
* To passively log memory usage when `log_resources` is given, the controller now calls `log()` as a side effect in most controller methods, with throttling to preserve speed.
* Return a status and status code from `pop()` etc.
* New internal function `as_monad()` makes error reporting more consistent.
* Use `path.expand()` on local log files.
* Switch to `Rscript`.
* Print session info from `crew_worker()` before attempting to dial into `mirai`.
* Add `asyncdial = FALSE` back to `mirai::daemon()` call.
* Deprecate `local_log_directory` and `local_log_join` in favor of `crew_options_local()` and the `options_local` argument.
* Add `crew_options_metrics()` and the `options_metrics` argument for recording resource metrics (#178).
* Add a new `logging.Rmd` vignette to explain best practices for logging and resource usage metrics (#178).

# crew 0.9.5

* CRAN patch.

# crew 0.9.4

* Do not use extended tasks in Shiny vignette.
* Add a new `retry_tasks` argument with default `TRUE` (#170).
* Avoid Base64 encoding functions from `nanonext`

# crew 0.9.3

* Deprecate native `crew` promises in favor of native event-driven promises in `mirai` (#162).
* Update the `shiny.Rmd` and `promises.Rmd` vignette to recommend native event-driven `mirai` promises (#162).

# crew 0.9.2

* Use `.args` rather than `...` in `mirai::mirai()` to make sure arguments continue to be passed as local variables in `mirai` >= 0.13.1.9012.
* Add new controller methods `autoscale()`, `descale()`, and `started()` to facilitate different kinds of Shiny apps.
* Deprecate the `scale` and `throttle` methods of `controller$promise()`. `promise()` now always calls `autoscale()` to make sure one and only one auto-scaling loop is running asynchronously. Auto-scaling thus continues even after the promise resolves.
* Add a second example vignette that simulates coin flips.
* Add a new `error` argument to `collect()` (#166).

# crew 0.9.1

* Rewrite the async Shiny vignette with `crew` promises and Shiny extended tasks (#157, @jcheng5).
* Clarify the intent of `controller$promise(mode = "one")` in the vignette on promises (@jcheng5).
* Implement an `error` argument in `pop()` which may help with integration with `ExtendedTask` (@jcheng5).
* Handle task errors in the Shiny vignette (@jcheng5).

# crew 0.9.0

* Require `nanonext` >= 0.12.0 and `mirai` >= 0.12.0.
* Return to always re-launching backlogged inactive workers (#79, https://github.com/r-lib/mirai/discussions/95).
* Implement `push_backlog()` and `pop_backlog()` to manage cases when it is not desirable to push to saturated controllers (https://github.com/ropensci/targets/issues/1220).
* Invisibly return the `mirai` object of a task pushed to the controller. This allows users to interact with the task directly, e.g. to create a promise object with `promises::as.promise()` (#146, @jcheng5).
* Add a new `walk()` method for controllers and controller groups to submit tasks in batch and return control immediately without waiting for any task to complete (#148, @jcheng5).
* Revive the `collect()` method for popping multiple tasks at once (#148, @jcheng5).
* Add controller group methods `nonempty()`, `resolved()`, `unresolved()`, and `unpopped()` to help with #148.
* Make the `mirai` dispatcher error message extremely verbose.

# crew 0.8.0

* Configure workers to send themselves a termination signal if the connection to the dispatcher is broken (#141, @psychelzh). Huge thanks to @shikokuchuo for the support through https://github.com/r-lib/mirai/issues/87, https://github.com/r-lib/mirai/pull/88, and https://github.com/r-lib/nanonext/pull/25! The signal itself is platform-dependent and determined by the new function `crew_terminate_signal()`.
* Implement `crew_monitor_local()` to help users monitor and terminate local R processes created by `crew` and `mirai`.
* Implement new utility function `crew_terminate_process()` to terminate a process manually without resorting to `SIGKILL` on Windows.
* Throw a warning from `controller$map()` if at least one task threw one. `warnings = FALSE` suppresses this behavior.
* Set `output = TRUE` in `daemon()` so `stdout` and `stderr` streams print.
* Add new arguments `local_log_directory` and `local_log_join` to write to local log files.

# crew 0.7.0

* Migrate from `asyncdial` to `autoexit`.
* Use `Sys.info()[["user"]]` to get the user in `crew_clean()` (#135, @luwidmer).
* Use condition variables to wait for tasks more efficiently (#108).
* Because of #108, `controller$map()` can no longer be used if there are tasks waiting to be popped.
* Use a `cli` progress bar in `map()`.
* Encapsulate non-function `R6` members in the `private` list and add active bindings where interfaces are necessary (#137). Ad hoc tests that absolutely need to modify private objects can use `object$.__enclos_env__$private` trick, but these tests should be skipped on CRAN in case there is a change to `R6` that breaks this.
* Drop `.signal` from `mirai::mirai()` since all `mirai` tasks signal as of `mirai` version 0.11.2.9025.
* Implement `crew_throttle()`, a decoupled mechanism for throttling that can be applied to scaling.
* Bring back the `throttle` argument, powered by `crew_throttle()`.
* Retry `mirai::status()` again in `daemons_info()` and make it configurable using `seconds_interval` and `seconds_timeout` in both the client and the launcher (#128).

# crew 0.6.0

* Migrate checks to enforce features in version 0.5.0 for reverse dependencies.
* Drop check of backlogged workers, c.f. https://github.com/r-lib/mirai/discussions/63#discussioncomment-7051889 (#79, #124, @shikokuchuo).
* Deprecate `seconds_exit` because `exitlinger` in `mirai` is now obsolete (#125, @shikokuchuo).
* Use `mirai::nextget("cv")` to count unresolved tasks instead of looping through all the task objects (#131).
* Remove throttling and `collect()` in auto-scaling. Simplifies much of the code. Made possible by the efficiency gains in #131.
* Simplify `wait()`.
* `seconds_interval` in `map()` no longer defaults to `controller$client$seconds_interval`.
* `launcher$terminate_workers()` terminates one or more workers, and `launcher$terminate` terminates the whole launcher.
* Add infrastructure to let custom launcher plugins launch and terminate workers asynchronously (#133). Launchers can set a positive number in the `processes` field to set the number of local `mirai` daemons for asynchronous requests to launch and terminate the serious workers. Then, `launch_worker()` and `terminate_worker()` can optionally make use of `launcher$async$eval()` to send these asynchronous calls.

# crew 0.5.0

* Suppress interactive browser on Windows which launched on each worker previously (@psychelzh).
* Migrate to the new host/daemon nomenclature in `mirai` 0.9.1 (#96).
* Suppress `status()` retries (@shikokuchuo, #100).
* Implement `launch_max` to error out if workers repeatedly launch without completing any tasks (#101, @shikokuchuo, @multimeric).
* Detect discovered workers more robustly in `launcher$done()`.
* Add a new `algorithm` argument to the `push()`, `shove()`, and `map()` methods of controllers and controller groups, as well as arguments/fields in `crew_eval()` and its return value (#113, @shikokuchuo).
* As a default for pseudo-random number generation, leverage widely-spaced L'Ecuyer streams as supported by `mirai::nextstream()` (#115, @shikokuchuo).
* Move README documentation to vignettes.
* Add a new `crew_tls()` function for TLS configuration (#120).
* Deprecate the `tls_enable` and `tls_config` arguments of `crew_client()` etc. in favor of a `tls` argument which accepts `crew_tls()` objects (#120).

# crew 0.4.0

## Highlights

* Use the TLS capabilities of `nanonext` and `mirai`.
* Add a `map()` method to the controller class for functional programming (#93).
* Fix bug in `scale()` which launched too many workers when `active - demand` was less than 0.
* Add `crew_clean()` to help clean up any lingering dispatchers and workers from previous sessions.

## Other improvements

* Remove temporary controller summary workaround that ensured CRAN compatibility with `targets` 1.1.3.
* Add guardrails to make sure the controller is started for methods that require this. Part of this involved improving the error messages.
* Retry `daemons()` in `daemons_info()`.

# crew 0.3.0

* Track warnings properly (#81, @brendanf).
* Optimize heavily (#81, #83, @shikokuchuo, @brendanf).
* Use CRAN `nanonext` 0.9.0.
* Delegate the task pushing and collection logic to a new `R6` schedule class (#84).
* Delegate auto-scaling to the launcher (#84).
* Drastically simplify internal logic (#84).
* Rename "router" to "client" and change the function signature of `launch_worker()` (#84). Unfortunately neither of these changes is back-compatible for third-party launcher plugins. However, the controller interface is still back-compatible, so `targets` remains unaffected.
* Terminate "lost" workers on websocket rotation.
* Allow `NULL` result in `pop()$result[[1L]]`.
* Delegate summaries among the controller, schedule, launcher, and client.
* Default to the loopback address for the local controller.
* Use already cumulative `assigned` and `complete` stats from `mirai` 0.8.7.9029.
* Reorganize `pkgdown` reference and `Rd` families to clarify which parts are for end-users and which parts are for launcher plugin developers.
* Add `controller$shove()` and `schedule$list()` to support fast extensions for functional programming.
* Depend on `mirai` 0.9.0 or greater.

# crew 0.2.1

* Use `packageStartupMessage()`.

# crew 0.2.0

* Use `.args` instead of `...` in `mirai::mirai()`.
* Use `set.seed()` instead of `withr::local_seed()` (#67, @shikokuchuo).
* Add a vignette to showcase integration with Shiny, co-authored with @dwoodie and with helpful suggestions by @shikokuchuo.
* Drop `rlang::duplicate()` (#72, @shikokuchuo).
* Do not poll dispatcher availability when starting router objects (@shikokuchuo).
* Implement a `saturated()` controller method to support `targets`.
* Add a `worker_index` column to controller summaries.
* When relaunching workers, prioritize those with unresolved tasks stuck at the NNG level (#75, @shikokuchuo).
* Always relaunch backlogged inactive workers (#79).
* Deprecate the `auto_scale` argument/field of controllers in favor of the `scale` argument of `push()`, `pop()`, and `wait()`.
* Throttle auto-scaling with interval `self$router$seconds_interval` (#76).
* Remove `clean()` and instead terminate lost workers on launch.
* Fix examples.
* Add a `collect` argument to `pop()`.

# crew 0.1.1

* Ensure checks pass on CRAN.

# crew 0.1.0

* Remove `asyncdial` from call to `mirai::daemons()`.
* Explain vision in the launcher plugin vignette.
* Remove the superfluous `clean()` controller method.
* Clean up logic for `scale()` and `launch()` controller methods.
* Add a new `inactive()` launcher method to help with the above.
* Eliminate superfluous worker launches: clean up the controller logic, and check worker discoveries before worker connections (#51).
* Simplify the launcher plugin interface (#54).
* Commit to using the fast `nanonext::.unresolved()` (#49).
* Rename the `callr` launcher to the local process launcher (#56).
* Switch from SHA224 to SHA1 as the hash for random tokens and names (#59, @shikokuchuo).
* Open and confirm bus socket listeners asynchronously in the launcher abstract class (#58).
* Use condition variables to test the connection status of worker instances (#57, @shikokuchuo).
* Allow the dispatcher process to not exist so that the router will keep working if `mirai` implements the dispatcher another way (#60).
* Add a `substitute` argument to `push()` (#63).
* Remove dependency on custom NNG bus sockets, which significantly simplifies the internals, improves robustness, and smooths the integration with `mirai` (#61).
* Use `processx` instead of `callr` for the local process launcher (#64").
* Speed up `crew_eval()` by e.g. relying on `nanonext::mclock()` for the time.
* Add an `empty()` method to check if the controller or controller group is empty.
* Rely on `mirai` for globals cleanup, garbage collection, and global option resets (#65).
* Expose the package unloading capabilities of `mirai` (#65).
* Make monads classed environments and leave tibbles to the last minute in `pop()`.
* Rename `crew_wait()` to `crew_retry()`.
* Export `crew_assert()` and `crew_random_name()` to help launcher plugins.
* Rename the `listen()` method to `start()` in the router class.
* The `poll()` method now retries `mirai::daemons()` for robustness.
* `crew_retry()` gains a `max_tries` argument.
* Bug fix: stop setting `router$daemons` to `NULL` in `poll()` when the connection has a momentary problem.
* Simplify `listening()`.

# crew 0.0.5

* Write a `call()` method to make it easier to write custom launcher plugins.
* Add robust documentation and tests for custom launcher plugins.
* Use `as.character()` to make sure the `list` argument supplied to `rm()` is safe. Fixes a test that was failing on R-devel with `crew` version 0.0.4.
* Remove the `seconds_poll_high` and `seconds_poll_low` fields and arguments for compatibility with `mirai` development and release versions.
* Hard-code the `async_dial` field in the router for compatibility with `mirai` development and release versions.

# crew 0.0.4

* Adjust references and tests as requested by CRAN.

# crew 0.0.3

* First version.
