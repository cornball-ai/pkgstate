## Typed conditions. pkgstate_error inherits runix_error so callers can catch
## either the package class or the framework class.
stop_pkgstate <- function(..., class = character(), call. = sys.call(-1)) {
    stop(structure(
                   class = c(class, "pkgstate_error", "runix_error", "error",
                             "condition"),
                   list(message = paste0(...), call = call.)
        ))
}
