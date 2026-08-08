# Scaffold sanity: metadata parses and declares the read-only scope.
d <- read.dcf(system.file("DESCRIPTION", package = "pkgstate"))
expect_equal(unname(d[, "Package"]), "pkgstate")
expect_equal(unname(d[, "OS_type"]), "unix")
