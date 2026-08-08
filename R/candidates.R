#' Installed and candidate versions of packages
#'
#' For each requested package, returns dpkg's installed version and apt's
#' candidate version (the version an upgrade would install), as reported by
#' the policy engine.
#'
#' @param packages Character vector of package names, length >= 1. The
#'   all-known-packages form is reserved for a future native libapt backend.
#' @return A data.frame with columns \code{package}, \code{installed}, and
#'   \code{candidate}. Version columns are \code{NA} where apt reports
#'   \code{(none)}. Unknown package names yield zero rows and are not an
#'   error.
#' @examples
#' \dontrun{
#' apt_candidates(c("dpkg", "bash"))
#' }
#' @export
apt_candidates <- function(packages) {
    outputs <- run_policy_chunks(packages)
    out <- do.call(rbind, c(lapply(outputs, parse_policy_candidates),
                            list(make.row.names = FALSE)))
    if (is.null(out)) {
        out <- parse_policy_candidates(character())
    }
    out
}

## Shared bridge plumbing for apt-cache policy queries: validates the
## packages argument, chunks it (1000 names per invocation), and returns
## one output vector per chunk.
run_policy_chunks <- function(packages) {
    if (is.null(packages)) {
        stop_pkgstate("packages = NULL (all known packages) is reserved for ",
                      "the native libapt backend; pass explicit package names")
    }
    if (!is.character(packages) || length(packages) == 0L ||
        anyNA(packages) || !all(nzchar(packages))) {
        stop_pkgstate("packages must be a character vector of package names")
    }
    packages <- unique(packages)
    chunks <- split(packages, ceiling(seq_along(packages) / 1000))
    lapply(chunks, function(chunk) {
        res <- runner()("apt-cache",
            c("policy", vapply(chunk, shQuote, character(1))))
        if (res$status != 0L) {
            stop_pkgstate("apt-cache policy failed with status ", res$status)
        }
        res$output
    })
}

## Pure parser for the per-package Installed:/Candidate: header lines of
## apt-cache policy output. Shares the line taxonomy of
## parse_policy_packages(); unrecognized lines are an error.
parse_policy_candidates <- function(lines) {
    package <- installed <- candidate <- character()
    n <- 0L
    for (i in seq_along(lines)) {
        line <- lines[i]
        if (!nzchar(line)) {
            next
        }
        if (grepl("^[^ ].*:$", line)) {
            n <- n + 1L
            package[n] <- sub(":$", "", line)
            installed[n] <- candidate[n] <- NA_character_
        } else if (grepl("^  Installed: ", line)) {
            if (n == 0L) {
                stop_pkgstate("Installed line before any package header (line ",
                              i, ")", class = "runix_parse_error")
            }
            installed[n] <- sub("^  Installed: ", "", line)
        } else if (grepl("^  Candidate: ", line)) {
            if (n == 0L) {
                stop_pkgstate("Candidate line before any package header (line ",
                              i, ")", class = "runix_parse_error")
            }
            candidate[n] <- sub("^  Candidate: ", "", line)
        } else if (grepl("^  Version table:", line) ||
            grepl("^  Package pin: ", line) ||
            grepl("^ \\*\\*\\* ", line) || grepl("^     [^ ]", line) ||
            grepl("^        -?[0-9]+ ", line)) {
            next
        } else {
            stop_pkgstate("unparseable apt-cache policy line (line ", i,
                          "): ", line, class = "runix_parse_error")
        }
    }
    if (n > 0L && (anyNA(installed) || anyNA(candidate))) {
        stop_pkgstate("package block missing Installed or Candidate line",
                      class = "runix_parse_error")
    }
    installed[installed == "(none)"] <- NA_character_
    candidate[candidate == "(none)"] <- NA_character_
    data.frame(package = package, installed = installed,
               candidate = candidate, stringsAsFactors = FALSE)
}
