# crew 0.0.5.9000

* Remove `asyncdial` from call to `mirai::daemons()`.
* Explain vision in the launcher plugin vignette.
* Remove the superfluous `clean()` controller method.
* Clean up logic for `scale()` and `launch()` controller methods.
* Add a new `inactive()` launcher method to help with the above.
* Reduce superfluous worker launches using cleaner controller logic (#51).
* Eliminate the remaining superfluous worker launches with a last-minute check for an existing connection in the launcher (#51).
* Simplify the launcher plugin interface (#54).

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
