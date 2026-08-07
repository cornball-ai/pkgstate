# Scaffold sanity: metadata parses and declares the read-only scope.
d <- read.dcf(system.file("DESCRIPTION", package = "rdpkg"))
expect_equal(unname(d[, "Package"]), "rdpkg")
expect_equal(unname(d[, "OS_type"]), "unix")
