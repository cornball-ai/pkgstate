.onLoad <- function(libname, pkgname) {
    ## Declare the retryable class in the shared runix registry so consumers
    ## (e.g. rctl) classify a cache race via runix::is_retryable() without
    ## hardcoding the class string. register_retryable() is idempotent (union),
    ## so repeated loads are harmless.
    runix::register_retryable("pkgstate_cache_race")
    invisible()
}
