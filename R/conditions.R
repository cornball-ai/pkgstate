## Typed conditions built on the runix core taxonomy. stop_pkgstate() wraps
## runix::runix_abort() so pkgstate_error inherits runix_error and callers can
## catch either the package class or the framework class. The subclass vector
## prepends any caller-supplied class to pkgstate_error, preserving the
## historical c(class, "pkgstate_error", "runix_error", "error", "condition").
stop_pkgstate <- function(..., class = character(), call. = sys.call(-1)) {
    cl <- call. # force the default in this frame so the call is the caller's
    runix::runix_abort(paste0(...), subclass = c(class, "pkgstate_error"),
                       call = cl)
}
