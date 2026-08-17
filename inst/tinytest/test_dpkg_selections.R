# dpkg_selections(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "pkgstate")
}
fx <- function(f) readLines(file.path(fixdir, f))

fake <- function(lines, status = 0L) {
    function(cmd, args) list(status = status, output = lines)
}

cols <- c("package", "architecture", "selection")

# --- Recorded output parses to the contracted shape ---

old <- pkgstate:::set_runner(fake(fx("dpkg-query-selections.txt")))
df <- dpkg_selections()
pkgstate:::set_runner(old)

expect_inherits(df, "data.frame")
expect_equal(names(df), cols)
expect_true(nrow(df) > 5L)
expect_true(all(df$selection %in%
    c("install", "hold", "deinstall", "purge", "unknown")))
expect_true("hold" %in% df$selection)
expect_false(any(vapply(df, is.factor, logical(1))))

# --- The packages filter selects a subset, same columns, no error on unknown ---

pkgstate:::set_runner(fake(fx("dpkg-query-selections.txt")))
sub <- dpkg_selections(packages = c("libc6", "dpkg", "no-such-pkg-xyzzy"))
pkgstate:::set_runner(old)
expect_equal(sort(unique(sub$package)), c("dpkg", "libc6"))
expect_equal(names(sub), cols)
expect_equal(sub$selection[sub$package == "libc6"], "hold")

# --- A multiarch package keeps one row per architecture ---

pkgstate:::set_runner(fake(fx("dpkg-query-selections.txt")))
z <- dpkg_selections(packages = "zlib1g")
pkgstate:::set_runner(old)
expect_equal(nrow(z), 2L)
expect_equal(sort(z$architecture), c("amd64", "i386"))

# --- Fail-closed on malformed output ---

pkgstate:::set_runner(fake(fx("dpkg-query-selections-malformed.txt")))
e <- tryCatch(dpkg_selections(), error = identity)
pkgstate:::set_runner(old)
expect_inherits(e, "runix_parse_error")
expect_inherits(e, "pkgstate_error")
expect_inherits(e, "runix_error")

# --- Non-zero exit status is an error, not empty ---

pkgstate:::set_runner(fake(character(), status = 2L))
e <- tryCatch(dpkg_selections(), error = identity)
pkgstate:::set_runner(old)
expect_inherits(e, "pkgstate_error")

# --- Empty output means a zero-row frame with the same columns ---

pkgstate:::set_runner(fake(character()))
df0 <- dpkg_selections()
pkgstate:::set_runner(old)
expect_equal(nrow(df0), 0L)
expect_equal(names(df0), cols)

# --- Live smoke tests ---

if (at_home()) {
    live <- dpkg_selections()
    expect_equal(names(live), cols)
    expect_true(nrow(live) > 100L)
    expect_true("dpkg" %in% live$package)
    # every want value is in dpkg's closed selection vocabulary (a box may have
    # no held package, so we assert the value set, not that "hold" is present)
    expect_true(all(live$selection %in%
        c("install", "hold", "deinstall", "purge", "unknown")))
    # the filter round-trips against a package known to be present
    one <- dpkg_selections(packages = "dpkg")
    expect_true(nrow(one) >= 1L)
    expect_true(all(one$package == "dpkg"))
}
