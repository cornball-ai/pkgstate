## Typed conditions. rdpkg_error inherits runix_error so callers can catch
## either the package class or the framework class.
stop_rdpkg <- function(..., class = character(), call. = sys.call(-1)) {
    stop(structure(
        class = c(class, "rdpkg_error", "runix_error", "error", "condition"),
        list(message = paste0(...), call = call.)
    ))
}
