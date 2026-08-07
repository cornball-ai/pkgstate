# apt_candidates(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rdpkg")
}
fx <- function(f) readLines(file.path(fixdir, f))

fake <- function(lines, status = 0L) {
    function(cmd, args) list(status = status, output = lines)
}

cols <- c("package", "installed", "candidate")

# --- Recorded real output parses to the contracted shape ---

old <- rdpkg:::set_runner(fake(fx("apt-cache-policy-pkgs.txt")))
on.exit(rdpkg:::set_runner(old), add = TRUE)
df <- apt_candidates(c("dpkg", "bash", "linux-image-6.14.0-1014-oem"))
rdpkg:::set_runner(old)

expect_equal(names(df), cols)
d <- df[df$package == "dpkg", ]
expect_equal(d$installed, d$candidate)
expect_true(nzchar(d$installed))
k <- df[df$package == "linux-image-6.14.0-1014-oem", ]
expect_true(is.na(k$installed))
expect_true(is.na(k$candidate))

# --- Phased fixture: installed and candidate differ ---

rdpkg:::set_runner(fake(fx("apt-cache-policy-phased.txt")))
p <- apt_candidates("alsa-ucm-conf")
rdpkg:::set_runner(old)
expect_equal(p$installed, "1.2.10-1ubuntu5.13")
expect_equal(p$candidate, "1.2.10-1ubuntu5.14")

# --- Fail-closed on malformed output ---

rdpkg:::set_runner(fake(fx("apt-cache-policy-malformed.txt")))
e <- tryCatch(apt_candidates("dpkg"), error = identity)
rdpkg:::set_runner(old)
expect_inherits(e, "runix_parse_error")

# --- Unknown packages yield zero rows ---

rdpkg:::set_runner(fake(character()))
df0 <- apt_candidates("no-such-package-xyzzy")
rdpkg:::set_runner(old)
expect_equal(nrow(df0), 0L)
expect_equal(names(df0), cols)

# --- Input validation (shared plumbing) ---

expect_error(apt_candidates(NULL))
expect_error(apt_candidates(character(0)))
expect_error(apt_candidates(NA_character_))

# --- Live smoke tests ---

if (at_home()) {
    live <- apt_candidates(c("dpkg", "bash"))
    expect_equal(nrow(live), 2L)
    expect_true(all(nzchar(live$installed)))
    expect_true(all(nzchar(live$candidate)))
}
