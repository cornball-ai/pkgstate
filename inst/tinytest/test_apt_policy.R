# apt_policy(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "pkgstate")
}
fx <- function(f) readLines(file.path(fixdir, f))

fake2 <- function(global, pkgs) {
    function(cmd, args) {
        if (length(args) == 1L) {
            list(status = 0L, output = global)
        } else {
            list(status = 0L, output = pkgs)
        }
    }
}

vcols <- c("version", "version_priority", "priority", "origin", "site",
    "suite", "component", "installed")

# --- Real pinned-out package (recorded): effective version priority -1
# --- differs from the source priority 500 ---

old <- pkgstate:::set_runner(fake2(fx("apt-cache-policy-global.txt"),
    fx("apt-cache-policy-verprio.txt")))
p <- apt_policy("nsight-compute")
pkgstate:::set_runner(old)

expect_equal(p$package, "nsight-compute")
expect_true(is.na(p$installed))
expect_true(is.na(p$candidate))
expect_true(is.na(p$pin))
expect_equal(names(p$versions), vcols)
expect_equal(p$versions$version_priority, -1L)
expect_equal(p$versions$priority, 500L)
expect_equal(p$versions$component, "multiverse")

# --- Synthetic "Package pin:" block (apt's documented format; no real
# --- package pin exists on the recording machine) ---

pkgstate:::set_runner(fake2(fx("apt-cache-policy-global.txt"),
    fx("apt-cache-policy-pin-synthetic.txt")))
pp <- apt_policy("pinnedpkg")
pkgstate:::set_runner(old)

expect_equal(pp$pin, "3.0")
expect_equal(pp$installed, "1.0")
expect_equal(pp$candidate, "3.0")
expect_true(all(pp$versions$version_priority == 990L))

# --- Pin lines are tolerated by the bulk parsers too ---

pkgstate:::set_runner(fake2(fx("apt-cache-policy-global.txt"),
    fx("apt-cache-policy-pin-synthetic.txt")))
o <- apt_origins("pinnedpkg")
cnd <- apt_candidates("pinnedpkg")
pkgstate:::set_runner(old)
expect_equal(nrow(o), 2L)
expect_equal(cnd$candidate, "3.0")

# --- Unknown package is a typed error here (diagnostic view) ---

pkgstate:::set_runner(fake2(fx("apt-cache-policy-global.txt"), character()))
e <- tryCatch(apt_policy("no-such-package-xyzzy"), error = identity)
pkgstate:::set_runner(old)
expect_inherits(e, "pkgstate_unknown_package")
expect_inherits(e, "pkgstate_error")

# --- Input validation ---

expect_error(apt_policy(c("a", "b")))
expect_error(apt_policy(NA_character_))
expect_error(apt_policy(""))

# --- Live smoke tests ---

if (at_home()) {
    live <- apt_policy("dpkg")
    expect_equal(live$package, "dpkg")
    expect_true(nzchar(live$installed))
    expect_equal(live$installed, live$candidate)
    expect_true(is.na(live$pin))
    expect_true(nrow(live$versions) >= 2L)
    expect_equal(names(live$versions), vcols)
    expect_true(all(live$versions$version_priority %in% c(-1L, 100L, 500L,
        990L, 1001L) | live$versions$version_priority > 0L))
}
