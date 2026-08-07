## Injectable runner: exported functions never call system2() directly.
## Tests replace the runner via set_runner(); set_runner(NULL) restores the
## default. The default enforces LC_ALL=C and fails closed on missing tools.

.runner_state <- new.env(parent = emptyenv())

runner <- function() {
    run <- .runner_state$run
    if (is.null(run)) run_system else run
}

set_runner <- function(run = NULL) {
    old <- .runner_state$run
    .runner_state$run <- run
    invisible(old)
}

## system2() invokes a shell, so callers must shQuote() any argument that
## contains characters the shell would interpret (dpkg-query format strings,
## user-supplied patterns).
run_system <- function(cmd, args) {
    if (Sys.which(cmd) == "") {
        stop_rdpkg("backend tool not found: ", cmd,
            class = "rdpkg_missing_tool")
    }
    out <- suppressWarnings(
        system2(cmd, args, stdout = TRUE, stderr = FALSE, env = "LC_ALL=C")
    )
    status <- attr(out, "status")
    list(
        status = if (is.null(status)) 0L else as.integer(status),
        output = as.character(out)
    )
}
