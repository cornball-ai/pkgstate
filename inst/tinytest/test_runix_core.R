## Compatibility after adopting the runix core: the condition taxonomy, the
## injectable-runner hook other tests depend on, and the retryability
## registration all behave as before.

## --- injectable runner round-trips (the fixture hook the suite relies on) ---
fake <- function(cmd, args) list(status = 0L, output = "x")
old <- pkgstate:::set_runner(fake)
expect_identical(pkgstate:::runner(), fake)
pkgstate:::set_runner(old)
expect_false(identical(pkgstate:::runner(), fake))

## --- typed conditions keep the full class taxonomy and message ---
e <- tryCatch(
    pkgstate:::stop_pkgstate("boom", class = "pkgstate_unknown_package"),
    condition = function(c) c)
expect_inherits(e, "pkgstate_unknown_package")
expect_inherits(e, "pkgstate_error")
expect_inherits(e, "runix_error")
expect_equal(conditionMessage(e), "boom")

## a bare stop_pkgstate() (no class) still lands pkgstate_error/runix_error
e0 <- tryCatch(pkgstate:::stop_pkgstate("bare"), condition = function(c) c)
expect_inherits(e0, "pkgstate_error")
expect_inherits(e0, "runix_error")

## --- the default runner fails closed on a missing tool, pkgstate-typed ---
mt <- tryCatch(
    pkgstate:::run_system("pkgstate-no-such-tool-xyz", character()),
    condition = function(c) c)
expect_inherits(mt, "pkgstate_missing_tool")
expect_inherits(mt, "pkgstate_error")
expect_inherits(mt, "runix_error")
expect_equal(conditionMessage(mt),
             "backend tool not found: pkgstate-no-such-tool-xyz")

## --- .onLoad registered the cache-race class in the shared runix registry ---
## (codex gate: classification works via the normal load path, not a manual
## registry poke.)
cache_race <- structure(
    class = c("pkgstate_cache_race", "pkgstate_error", "runix_error",
              "error", "condition"),
    list(message = "cache moved"))
expect_true(runix::is_retryable(cache_race))

## a non-retryable pkgstate error stays non-retryable
plain <- structure(
    class = c("pkgstate_error", "runix_error", "error", "condition"),
    list(message = "nope"))
expect_false(runix::is_retryable(plain))
