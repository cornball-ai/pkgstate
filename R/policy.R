#' Full apt policy view for one package
#'
#' The single-package diagnostic view: where the bulk views report data
#' (\code{\link{apt_origins}}: per-source rows; \code{\link{apt_candidates}}:
#' resolution results), \code{apt_policy()} explains why resolution went the
#' way it did — the package pin when present, and each version's effective
#' priority (the number pins alter, which can be negative), alongside its
#' sources.
#'
#' @param package A single package name (name, or name:arch).
#' @return A list with elements \code{package}, \code{installed},
#'   \code{candidate} (versions, NA when \code{(none)}), \code{pin} (the
#'   "Package pin:" version, NA when unpinned), and \code{versions} — a
#'   data.frame with columns \code{version}, \code{version_priority}
#'   (effective), \code{priority} (source), \code{origin}, \code{site},
#'   \code{suite}, \code{component}, \code{installed}.
#' @examples
#' \dontrun{
#' p <- apt_policy("dpkg")
#' p$versions
#' }
#' @export
apt_policy <- function(package) {
    if (!is.character(package) || length(package) != 1L ||
        is.na(package) || !nzchar(package)) {
        stop_pkgstate("package must be a single package name")
    }
    outputs <- run_policy_chunks(package)
    cand <- parse_policy_candidates(outputs[[1L]])
    if (nrow(cand) == 0L) {
        stop_pkgstate("no such package known to apt: ", package,
                      class = "pkgstate_unknown_package")
    }
    pins <- parse_policy_pins(outputs[[1L]])
    rows <- joined_policy_rows(outputs)
    versions <- rows[rows$package == cand$package[1L],
        c("version", "verprio", "priority", "origin", "site", "suite",
            "component", "installed")]
    names(versions)[names(versions) == "verprio"] <- "version_priority"
    rownames(versions) <- NULL
    list(
         package = cand$package[1L],
         installed = cand$installed[1L],
         candidate = cand$candidate[1L],
         pin = pins$pin[match(cand$package[1L], pins$package)],
         versions = versions
    )
}

## Pure parser for the optional "Package pin:" header line, sharing the
## line taxonomy of parse_policy_candidates(). One row per package block;
## pin is NA when the block has no pin line.
parse_policy_pins <- function(lines) {
    package <- pin <- character()
    n <- 0L
    for (i in seq_along(lines)) {
        line <- lines[i]
        if (!nzchar(line)) {
            next
        }
        if (grepl("^[^ ].*:$", line)) {
            n <- n + 1L
            package[n] <- sub(":$", "", line)
            pin[n] <- NA_character_
        } else if (grepl("^  Package pin: ", line)) {
            if (n == 0L) {
                stop_pkgstate("pin line before any package header (line ", i,
                              ")", class = "runix_parse_error")
            }
            pin[n] <- sub("^  Package pin: ", "", line)
        } else if (grepl("^  (Installed|Candidate|Version table):", line) ||
            grepl("^ \\*\\*\\* ", line) || grepl("^     [^ ]", line) ||
            grepl("^        -?[0-9]+ ", line)) {
            next
        } else {
            stop_pkgstate("unparseable apt-cache policy line (line ", i, "): ",
                          line, class = "runix_parse_error")
        }
    }
    data.frame(package = package, pin = pin, stringsAsFactors = FALSE)
}
