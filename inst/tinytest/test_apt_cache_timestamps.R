# apt_cache_timestamps(): tempdir fixtures (always run) + live smoke.

d <- tempfile("lists")
dir.create(d)
f1 <- file.path(d, "archive.ubuntu.com_ubuntu_dists_noble_InRelease")
f2 <- file.path(d, "archive.ubuntu.com_ubuntu_dists_noble_main_Packages")
writeLines("x", f1)
writeLines("x", f2)
t1 <- as.POSIXct("2026-08-01 10:00:00", tz = "UTC")
t2 <- as.POSIXct("2026-08-05 10:00:00", tz = "UTC")
Sys.setFileTime(f1, t1)
Sys.setFileTime(f2, t2)
status <- tempfile("status")
writeLines("Package: x", status)
Sys.setFileTime(status, t1)

ts <- apt_cache_timestamps(lists_dir = d, status_file = status)
expect_inherits(ts$lists_updated, "POSIXct")
expect_equal(attr(ts$lists_updated, "tzone"), "UTC")
expect_equal(as.numeric(ts$lists_updated), as.numeric(t2))
expect_equal(as.numeric(ts$status_changed), as.numeric(t1))

# lock files and subdirectories are not index stamps
dir.create(file.path(d, "partial"))
writeLines("x", file.path(d, "lock"))
ts2 <- apt_cache_timestamps(lists_dir = d, status_file = status)
expect_equal(as.numeric(ts2$lists_updated), as.numeric(t2))

# empty lists dir means never updated: NA, not an error
d2 <- tempfile("empty")
dir.create(d2)
ts3 <- apt_cache_timestamps(lists_dir = d2, status_file = status)
expect_true(is.na(ts3$lists_updated))
expect_inherits(ts3$lists_updated, "POSIXct")

# missing paths are errors
e <- tryCatch(apt_cache_timestamps(lists_dir = tempfile(),
    status_file = status), error = identity)
expect_inherits(e, "rdpkg_error")
e <- tryCatch(apt_cache_timestamps(lists_dir = d,
    status_file = tempfile()), error = identity)
expect_inherits(e, "rdpkg_error")

unlink(c(d, d2), recursive = TRUE)
unlink(status)

# --- Live smoke tests ---

if (at_home()) {
    live <- apt_cache_timestamps()
    expect_false(is.na(live$lists_updated))
    expect_false(is.na(live$status_changed))
    expect_true(live$status_changed > as.POSIXct("2020-01-01", tz = "UTC"))
    expect_equal(attr(live$lists_updated, "tzone"), "UTC")
}
