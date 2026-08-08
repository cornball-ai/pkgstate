#' Installed packages from the dpkg database
#'
#' Queries the dpkg status database and returns one row per package entry
#' known to dpkg, including entries in states other than fully installed
#' (for example \code{config-files} left by removed-but-not-purged packages).
#'
#' @return A data.frame with columns \code{package}, \code{version},
#'   \code{architecture}, and \code{status}. \code{status} is dpkg's status
#'   word, verbatim (e.g. \code{"installed"}, \code{"config-files"}).
#' @examples
#' \dontrun{
#' pkgs <- dpkg_installed()
#' pkgs[pkgs$status != "installed", ]
#' }
#' @export
dpkg_installed <- function() {
    fmt <- "${Package}\t${Version}\t${Architecture}\t${db:Status-Status}\n"
    res <- runner()("dpkg-query", c("-W", shQuote(paste0("-f=", fmt))))
    if (res$status != 0L) {
        stop_pkgstate("dpkg-query -W failed with status ", res$status)
    }
    parse_dpkg_w(res$output)
}

## Native dpkg architecture, for building name:arch query spellings.
native_arch <- function() {
    res <- runner()("dpkg", "--print-architecture")
    if (res$status != 0L || length(res$output) < 1L ||
        !nzchar(trimws(res$output[1L]))) {
        stop_pkgstate("dpkg --print-architecture failed")
    }
    trimws(res$output[1L])
}

## Pure parser, separated from the runner so fixture tests exercise it
## offline. Fail-closed: any line that is not exactly 4 tab-separated
## fields is an error, never a guess.
parse_dpkg_w <- function(lines) {
    lines <- lines[nzchar(lines)]
    if (length(lines) == 0L) {
        return(data.frame(package = character(), version = character(),
                          architecture = character(), status = character(),
                          stringsAsFactors = FALSE))
    }
    parts <- strsplit(lines, "\t", fixed = TRUE)
    bad <- which(lengths(parts) != 4L)
    if (length(bad) > 0L) {
        stop_pkgstate(
                      "unparseable dpkg-query output (line ", bad[1L], " of ",
                      length(lines), ")",
                      class = "runix_parse_error"
        )
    }
    m <- matrix(unlist(parts, use.names = FALSE), ncol = 4L, byrow = TRUE)
    data.frame(
               package = m[, 1L], version = m[, 2L],
               architecture = m[, 3L], status = m[, 4L],
               stringsAsFactors = FALSE
    )
}
