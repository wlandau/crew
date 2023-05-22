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
