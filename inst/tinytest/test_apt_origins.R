# apt_origins(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rdpkg")
}
fx <- function(f) readLines(file.path(fixdir, f))

# The runner sees two call shapes: args "policy" alone (global table) and
# args c("policy", <quoted names>) (per-package tables).
fake2 <- function(global, pkgs, counter = NULL) {
    function(cmd, args) {
        if (!is.null(counter)) {
            counter$n <- counter$n + 1L
        }
        if (length(args) == 1L) {
            list(status = 0L, output = global)
        } else {
            list(status = 0L, output = pkgs)
        }
    }
}

cols <- c("package", "version", "priority", "origin", "site", "suite",
    "component", "installed")

# --- Recorded real output parses and joins to the contracted shape ---

old <- rdpkg:::set_runner(fake2(fx("apt-cache-policy-global.txt"),
    fx("apt-cache-policy-pkgs.txt")))
on.exit(rdpkg:::set_runner(old), add = TRUE)
df <- apt_origins(c("dpkg", "bash", "linux-image-6.14.0-1014-oem"))
rdpkg:::set_runner(old)

expect_inherits(df, "data.frame")
expect_equal(names(df), cols)
expect_true(all(c("dpkg", "bash", "linux-image-6.14.0-1014-oem")
    %in% df$package))

# dpkg's installed version carries archive sources plus the status
# pseudo-source, all flagged installed.
ddf <- df[df$package == "dpkg" & df$installed, ]
expect_true(nrow(ddf) >= 2L)
now <- ddf[ddf$suite == "now", ]
expect_equal(nrow(now), 1L)
expect_equal(now$origin, "")
expect_equal(now$site, "")
expect_equal(now$priority, 100L)
archive <- ddf[ddf$suite != "now", ]
expect_true(all(archive$origin == "Ubuntu"))
expect_true(all(archive$component == "main"))
expect_true(all(nzchar(archive$site)))
expect_true(all(archive$priority == 500L))

# dpkg's non-installed archive version is present and not flagged.
expect_true(any(df$package == "dpkg" & !df$installed))

# The config-files kernel image: status-only, not installed.
kdf <- df[df$package == "linux-image-6.14.0-1014-oem", ]
expect_equal(nrow(kdf), 1L)
expect_equal(kdf$suite, "now")
expect_false(kdf$installed)

# --- Phased-update annotation on version lines is tolerated and ignored ---

p <- rdpkg:::parse_policy_packages(fx("apt-cache-policy-phased.txt"))
expect_true("1.2.10-1ubuntu5.14" %in% p$version)
expect_true(all(p$package == "alsa-ucm-conf"))
expect_false(any(p$installed & p$version == "1.2.10-1ubuntu5.14"))

# --- Exact-path repos (e.g. the CRAN/r2u apt repo) parse in the global
# --- table: dist with trailing slash, no component, no arch ---

g <- rdpkg:::parse_policy_global(fx("apt-cache-policy-global.txt"))
expect_true(any(grepl("cloud.r-project.org", g$key, fixed = TRUE)))

# --- Unknown packages yield zero rows with contracted columns ---

rdpkg:::set_runner(fake2(fx("apt-cache-policy-global.txt"), character()))
df0 <- apt_origins("no-such-package-xyzzy")
rdpkg:::set_runner(old)
expect_equal(nrow(df0), 0L)
expect_equal(names(df0), cols)

# --- Fail-closed on malformed version-table output ---

rdpkg:::set_runner(fake2(fx("apt-cache-policy-global.txt"),
    fx("apt-cache-policy-malformed.txt")))
e <- tryCatch(apt_origins("dpkg"), error = identity)
rdpkg:::set_runner(old)
expect_inherits(e, "runix_parse_error")
expect_inherits(e, "rdpkg_error")

# --- Input validation ---

e <- tryCatch(apt_origins(NULL), error = identity)
expect_inherits(e, "rdpkg_error")
expect_true(grepl("native libapt backend", conditionMessage(e)))
expect_error(apt_origins(character(0)))
expect_error(apt_origins(NA_character_))
expect_error(apt_origins(c("dpkg", "")))

# --- Chunking: 1500 names means 1 global + 2 per-package calls ---

counter <- new.env()
counter$n <- 0L
rdpkg:::set_runner(fake2(fx("apt-cache-policy-global.txt"), character(),
    counter))
apt_origins(sprintf("p%04d", 1:1500))
rdpkg:::set_runner(old)
expect_equal(counter$n, 3L)

# --- Live smoke tests ---

if (at_home()) {
    live <- apt_origins(c("dpkg", "bash"))
    expect_equal(names(live), cols)
    expect_true("Ubuntu" %in% live$origin)
    expect_true("now" %in% live$suite)
    expect_true(all(live$package %in% c("dpkg", "bash")))

    # Acceptance-shaped: every dpkg-known package resolves to at least a
    # status row, in one pass over a sample of the installed set.
    smp <- unique(head(dpkg_installed()$package, 150L))
    o <- apt_origins(smp)
    expect_true(all(smp %in% o$package))
    expect_true(all(o$package %in% smp))
}
