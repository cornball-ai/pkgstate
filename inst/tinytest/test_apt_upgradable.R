# apt_upgradable(): fixture tests (always run) + live smoke (at_home only).

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "rdpkg")
}
fx <- function(f) readLines(file.path(fixdir, f))

# apt_upgradable() drives four call shapes; dispatch on command and args.
fake_multi <- function(installed, arch, global, policy) {
    function(cmd, args) {
        if (cmd == "dpkg-query") {
            list(status = 0L, output = installed)
        } else if (cmd == "dpkg") {
            list(status = 0L, output = arch)
        } else if (length(args) == 1L) {
            list(status = 0L, output = global)
        } else {
            list(status = 0L, output = policy)
        }
    }
}

cols <- c("package", "installed", "candidate", "origin", "site", "suite",
    "component", "security", "phased_percent")

# --- A phased upgrade shows candidate-available with pass-through
# --- percentage; same-version and (none) packages are excluded ---

installed_lines <- c(
    "alsa-ucm-conf\t1.2.10-1ubuntu5.13\tall\tinstalled",
    "dpkg\t1.22.6ubuntu6.6\tamd64\tinstalled"
)
old <- rdpkg:::set_runner(fake_multi(installed_lines, "amd64",
    fx("apt-cache-policy-global.txt"),
    c(fx("apt-cache-policy-phased.txt"), fx("apt-cache-policy-pkgs.txt"))))
up <- apt_upgradable()
rdpkg:::set_runner(old)

expect_equal(names(up), cols)
expect_equal(nrow(up), 1L)
expect_equal(up$package, "alsa-ucm-conf")
expect_equal(up$installed, "1.2.10-1ubuntu5.13")
expect_equal(up$candidate, "1.2.10-1ubuntu5.14")
expect_equal(up$origin, "Ubuntu")
expect_equal(up$suite, "noble-updates")
expect_equal(up$component, "main")
expect_false(up$security)
expect_equal(up$phased_percent, 40L)

# --- A candidate served from a security pocket flags security ---

sec_policy <- c(
    "fakepkg:",
    "  Installed: 1.0",
    "  Candidate: 2.0",
    "  Version table:",
    "     2.0 500",
    paste0("        500 http://security.ubuntu.com/ubuntu ",
        "noble-security/main amd64 Packages"),
    " *** 1.0 100",
    "        100 /var/lib/dpkg/status"
)
rdpkg:::set_runner(fake_multi("fakepkg\t1.0\tamd64\tinstalled", "amd64",
    fx("apt-cache-policy-global.txt"), sec_policy))
sec <- apt_upgradable()
rdpkg:::set_runner(old)
expect_equal(sec$package, "fakepkg")
expect_true(sec$security)
expect_equal(sec$suite, "noble-security")
expect_true(is.na(sec$phased_percent))

# --- security reflects the CANDIDATE version's sources only: an installed
# --- version from a security pocket must not taint the flag ---

mixed_policy <- c(
    "mixedpkg:",
    "  Installed: 1.0",
    "  Candidate: 2.0",
    "  Version table:",
    "     2.0 500",
    paste0("        500 http://archive.ubuntu.com/ubuntu ",
        "noble-updates/main amd64 Packages"),
    " *** 1.0 500",
    paste0("        500 http://security.ubuntu.com/ubuntu ",
        "noble-security/main amd64 Packages"),
    "        100 /var/lib/dpkg/status"
)
rdpkg:::set_runner(fake_multi("mixedpkg\t1.0\tamd64\tinstalled", "amd64",
    fx("apt-cache-policy-global.txt"), mixed_policy))
mixed <- apt_upgradable()
rdpkg:::set_runner(old)
expect_equal(mixed$package, "mixedpkg")
expect_false(mixed$security)
expect_equal(mixed$suite, "noble-updates")

# --- Nothing upgradable means zero rows with contracted columns ---

rdpkg:::set_runner(fake_multi("dpkg\t1.22.6ubuntu6.6\tamd64\tinstalled",
    "amd64", fx("apt-cache-policy-global.txt"),
    fx("apt-cache-policy-pkgs.txt")))
none <- apt_upgradable()
rdpkg:::set_runner(old)
expect_equal(nrow(none), 0L)
expect_equal(names(none), cols)

# --- Candidate with no archive source is a loud error, not a guess ---

race_policy <- c(
    "ghostpkg:",
    "  Installed: 1.0",
    "  Candidate: 2.0",
    "  Version table:",
    " *** 1.0 100",
    "        100 /var/lib/dpkg/status"
)
rdpkg:::set_runner(fake_multi("ghostpkg\t1.0\tamd64\tinstalled", "amd64",
    fx("apt-cache-policy-global.txt"), race_policy))
e <- tryCatch(apt_upgradable(), error = identity)
rdpkg:::set_runner(old)
expect_inherits(e, "rdpkg_error")
expect_true(grepl("no archive source", conditionMessage(e)))

# --- Live smoke tests ---

if (at_home()) {
    live <- apt_upgradable()
    expect_equal(names(live), cols)
    expect_true(nrow(live) >= 1L)
    expect_true(all(live$candidate != live$installed))
    expect_true(all(is.na(live$phased_percent) |
        (live$phased_percent >= 0L & live$phased_percent <= 100L)))
    expect_true(all(nzchar(live$suite)))
    expect_true(is.logical(live$security))
}
