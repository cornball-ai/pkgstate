# Phase 1 acceptance (contract: runix docs/phase1-introspection-contracts.md):
# reproduce ubuntu-security-status's installed-package classification from
# pkgstate exported functions alone, validated against the machine-readable
# `pro security-status --format json` (which u-s-s itself defers to).
#
# The installed-package vector is passed to apt_origins() explicitly; the
# packages = NULL all-known form belongs to the future native libapt
# backend. Live-system test: runs at home only, and can race a concurrent
# apt transaction (unattended-upgrades) - rerun on mismatch.
#
# BOUNDARY: this comparison against `pro security-status` is a local
# acceptance test only. Never wire it into R CMD check or CI - those
# contexts are covered by the fixture tests alone.

if (!at_home()) {
    exit_file("acceptance test runs at home only")
}
if (Sys.which("pro") == "" || Sys.which("dpkg") == "") {
    exit_file("needs pro and dpkg")
}
if (!requireNamespace("janssonr", quietly = TRUE)) {
    exit_file("needs janssonr")
}

inst <- dpkg_installed()
inst <- inst[inst$status == "installed", ]
native <- trimws(system2("dpkg", "--print-architecture", stdout = TRUE))
qn <- unique(ifelse(inst$architecture %in% c(native, "all"),
    inst$package,
    paste0(inst$package, ":", inst$architecture)))

o <- apt_origins(qn)
cand <- apt_candidates(qn)

# Mirror uaclient's get_origin_for_installed_package(): walk the installed
# version's sources in order; if the installed version is status-only, fall
# back to the candidate version's sources (no candidate, or candidate ==
# installed, means "unknown"); first Ubuntu source's component wins;
# otherwise third-party.
inst_rows <- o[o$installed, ]
by_inst <- split(inst_rows, inst_rows$package)
by_all <- split(o, o$package)
cand_installed <- stats::setNames(cand$installed, cand$package)
cand_candidate <- stats::setNames(cand$candidate, cand$package)

classify <- function(name) {
    r <- by_inst[[name]]
    if (is.null(r)) {
        return("unknown")
    }
    if (nrow(r) == 1L) {
        cv <- cand_candidate[[name]]
        if (is.na(cv) || identical(cv, cand_installed[[name]])) {
            return("unknown")
        }
        a <- by_all[[name]]
        r <- a[a$version == cv, ]
    }
    r <- r[r$suite != "now", ]
    if (nrow(r) == 0L) {
        return("unknown")
    }
    ubuntu <- which(r$origin == "Ubuntu")
    if (length(ubuntu) > 0L) {
        return(r$component[ubuntu[1L]])
    }
    "third_party"
}
cls <- vapply(qn, classify, character(1))
counts <- table(factor(cls, levels = c("main", "restricted", "universe",
    "multiverse", "third_party", "unknown")))

ss <- janssonr::from_json(paste(
    system2("pro", c("security-status", "--format", "json"), stdout = TRUE),
    collapse = ""
))
s <- ss$summary

expect_equal(length(qn), s$num_installed_packages)
expect_equal(unname(counts[["main"]] + counts[["restricted"]]),
    s$num_main_packages + s$num_restricted_packages)
expect_equal(unname(counts[["universe"]] + counts[["multiverse"]]),
    s$num_universe_packages + s$num_multiverse_packages)
expect_equal(unname(counts[["third_party"]]), s$num_third_party_packages)
expect_equal(unname(counts[["unknown"]]), s$num_unknown_packages)
