## Injectable runner over the runix core (runix::new_runner). Exported
## functions never call system2() directly; tests replace the runner via
## set_runner() and set_runner(NULL) restores the default. The default
## enforces LC_ALL=C and fails closed on a missing tool with a pkgstate-typed
## condition (pkgstate_missing_tool / pkgstate_error / runix_error).
##
## On a Unix-alike, system2() concatenates the command and args into one
## command line run via /bin/sh, so an unquoted argument can be interpreted as
## shell syntax. Invariant: every non-literal argument (dpkg-query format
## strings, user-supplied patterns) is shQuote()d at its call site; the
## command name is quoted by system2() itself.
.pkgstate_runner <- runix::new_runner(
    default_env = "LC_ALL=C",
    missing_tool_subclass = c("pkgstate_missing_tool", "pkgstate_error"))

runner <- .pkgstate_runner$runner
set_runner <- .pkgstate_runner$set_runner
run_system <- .pkgstate_runner$run_system
