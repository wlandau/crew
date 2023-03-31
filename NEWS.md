# crew 0.0.5.9000

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
