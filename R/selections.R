#' dpkg selection (want) state
#'
#' Reads the dpkg selection -- the \dQuote{want} field of each package's status
#' triplet -- via \code{dpkg-query}. This is the hold state: a held package has
#' selection \code{"hold"}, a normally-installed one \code{"install"}. It is
#' distinct from \code{\link{dpkg_installed}}'s status word (\code{installed},
#' \code{config-files}), which is the current \emph{state}, not the desired
#' selection.
#'
#' @param packages Optional character vector of package names to filter to;
#'   \code{NULL} (default) returns every entry dpkg knows. Filtering is applied
#'   in R after the full read, so an unknown name simply yields no row rather
#'   than an error.
#' @return A data.frame with columns \code{package}, \code{architecture}, and
#'   \code{selection}. \code{selection} is dpkg's want word, verbatim:
#'   \code{"install"}, \code{"hold"}, \code{"deinstall"}, \code{"purge"}, or
#'   \code{"unknown"}.
#' @examples
#' \dontrun{
#' sel <- dpkg_selections()
#' sel[sel$selection == "hold", ]
#' }
#' @export
dpkg_selections <- function(packages = NULL) {
    fmt <- "${Package}\t${Architecture}\t${db:Status-Want}\n"
    res <- runner()("dpkg-query", c("-W", shQuote(paste0("-f=", fmt))))
    if (res$status != 0L) {
        stop_pkgstate("dpkg-query -W failed with status ", res$status)
    }
    df <- parse_dpkg_selections(res$output)
    if (!is.null(packages)) {
        df <- df[df$package %in% packages,, drop = FALSE]
        rownames(df) <- NULL
    }
    df
}

## dpkg's closed want vocabulary. Anything outside it is malformed output, not
## a new selection to pass through.
.DPKG_WANTS <- c("install", "hold", "deinstall", "purge", "unknown")

## Pure parser, separated from the runner so fixture tests exercise it offline.
## Fail-closed and total: an empty read is a zero-row frame, but within a
## non-empty read every line must be exactly 3 tab-separated fields -- a blank
## line is malformed and errors, never silently dropped -- and every selection
## must be one of .DPKG_WANTS.
parse_dpkg_selections <- function(lines) {
    if (length(lines) == 0L) {
        return(data.frame(package = character(), architecture = character(),
                          selection = character(), stringsAsFactors = FALSE))
    }
    parts <- strsplit(lines, "\t", fixed = TRUE)
    bad <- which(lengths(parts) != 3L)
    if (length(bad) > 0L) {
        stop_pkgstate(
                      "unparseable dpkg-query selection output (line ", bad[1L],
                      " of ", length(lines), ")",
                      class = "runix_parse_error"
        )
    }
    m <- matrix(unlist(parts, use.names = FALSE), ncol = 3L, byrow = TRUE)
    unknown_sel <- which(!m[, 3L] %in% .DPKG_WANTS)
    if (length(unknown_sel) > 0L) {
        stop_pkgstate(
                      "unexpected dpkg selection ", shQuote(m[unknown_sel[1L], 3L]),
                      " (line ", unknown_sel[1L], " of ", nrow(m),
                      "); expected one of ", paste(.DPKG_WANTS, collapse = "/"),
                      class = "runix_parse_error"
        )
    }
    data.frame(
               package = m[, 1L], architecture = m[, 2L],
               selection = m[, 3L], stringsAsFactors = FALSE
    )
}
