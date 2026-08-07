# dpkg_installed(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rdpkg")
}
fx <- function(f) readLines(file.path(fixdir, f))

fake <- function(lines, status = 0L) {
    function(cmd, args) list(status = status, output = lines)
}

cols <- c("package", "version", "architecture", "status")

# --- Recorded real output parses to the contracted shape ---

old <- rdpkg:::set_runner(fake(fx("dpkg-query-W.txt")))
on.exit(rdpkg:::set_runner(old), add = TRUE)
df <- dpkg_installed()
rdpkg:::set_runner(old)

expect_inherits(df, "data.frame")
expect_equal(names(df), cols)
expect_true(nrow(df) > 10L)
expect_true(all(nzchar(df$package)))
expect_true("config-files" %in% df$status)
expect_false(any(vapply(df, is.factor, logical(1))))

# --- Fail-closed on malformed output ---

rdpkg:::set_runner(fake(fx("dpkg-query-W-malformed.txt")))
e <- tryCatch(dpkg_installed(), error = identity)
rdpkg:::set_runner(old)
expect_inherits(e, "runix_parse_error")
expect_inherits(e, "rdpkg_error")
expect_inherits(e, "runix_error")

# --- Non-zero exit status is an error, not empty ---

rdpkg:::set_runner(fake(character(), status = 2L))
e <- tryCatch(dpkg_installed(), error = identity)
rdpkg:::set_runner(old)
expect_inherits(e, "rdpkg_error")

# --- Empty output means a zero-row frame with the same columns ---

rdpkg:::set_runner(fake(character()))
df0 <- dpkg_installed()
rdpkg:::set_runner(old)
expect_equal(nrow(df0), 0L)
expect_equal(names(df0), cols)

# --- Missing backend tool is a typed error ---

e <- tryCatch(rdpkg:::run_system("no-such-tool-xyzzy", character()),
    error = identity)
expect_inherits(e, "rdpkg_missing_tool")

# --- Live smoke tests ---

if (at_home()) {
    live <- dpkg_installed()
    expect_equal(names(live), cols)
    expect_true(nrow(live) > 100L)
    expect_true("dpkg" %in% live$package)
    expect_true(all(live$status %in% c(
        "installed", "config-files", "half-installed", "unpacked",
        "half-configured", "triggers-awaited", "triggers-pending",
        "not-installed"
    )))
}
