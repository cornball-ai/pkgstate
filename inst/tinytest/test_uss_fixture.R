# The tree-sitter-generated ubuntu-security-status classification fixture
# stays parseable and carries the expected tuples. Regenerate with
# tools/make-uss-fixture.R after an update-manager-core update; drift shows
# as a diff there, not as a silent change here.

fixdir <- if (dir.exists("fixtures")) {
    "fixtures"
} else {
    system.file("tinytest", "fixtures", package = "pkgstate")
}
tsv <- readLines(file.path(fixdir, "uss-origin-tuples.tsv"))
expect_true(any(grepl("^# update-manager-core .", tsv)))

tsv <- tsv[!startsWith(tsv, "#")]
parts <- strsplit(tsv, "\t", fixed = TRUE)
expect_true(length(parts) >= 15L)
expect_true(all(lengths(parts) == 2L))

nm <- vapply(parts, `[`, character(1), 1L)
expect_true(all(startsWith(nm, "suite_")))
expect_true(all(c("suite_main", "suite_main_security", "suite_esm_main")
    %in% nm))
