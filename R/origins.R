#' Origins of available package versions
#'
#' For each requested package, returns one row per (version, source) from
#' apt's policy tables: which archive each available version comes from,
#' with apt's Origin label, suite, and component. This is the raw material
#' for origin classification (main/universe/ESM/PPA/third-party), which is
#' deliberately left to callers.
#'
#' @param packages Character vector of package names, length >= 1. The
#'   all-known-packages form is reserved for a future native libapt backend.
#' @return A data.frame with columns \code{package}, \code{version},
#'   \code{priority}, \code{origin}, \code{site}, \code{suite},
#'   \code{component}, and \code{installed}. The dpkg status pseudo-source
#'   appears with \code{origin ""}, \code{site ""}, \code{suite "now"},
#'   \code{component ""}.
#'   \code{installed} marks the currently installed version. Unknown
#'   package names yield zero rows and are not an error.
#' @examples
#' \dontrun{
#' o <- apt_origins(c("dpkg", "bash"))
#' o[o$suite != "now", c("package", "version", "origin", "component")]
#' }
#' @export
apt_origins <- function(packages) {
    rows <- joined_policy_rows(run_policy_chunks(packages))
    rows[c("package", "version", "priority", "origin", "site", "suite",
            "component", "installed")]
}

## Internal single pass shared by apt_origins() and apt_upgradable():
## parses the per-package outputs, fetches and parses the global policy
## table, and joins release fields onto every source row. Carries the
## phased column (Phased-Update-Percentage of the version, NA when
## unannotated), which apt_origins() drops.
joined_policy_rows <- function(outputs) {
    res <- runner()("apt-cache", "policy")
    if (res$status != 0L) {
        stop_pkgstate("apt-cache policy failed with status ", res$status)
    }
    global <- parse_policy_global(res$output)

    rows <- do.call(rbind, c(lapply(outputs, parse_policy_packages),
                             list(make.row.names = FALSE)))

    if (nrow(rows) == 0L) {
        return(data.frame(
                          package = character(), version = character(),
                          priority = integer(), origin = character(),
                          site = character(), suite = character(),
                          component = character(), installed = logical(),
                          phased = integer(), verprio = integer(),
                          stringsAsFactors = FALSE
            ))
    }

    idx <- match(rows$key, global$key)
    if (anyNA(idx)) {
        stop_pkgstate("package source not present in the global policy table (",
                   rows$key[which(is.na(idx))[1L]],
                   "); the apt cache may have changed between queries - retry",
                   class = "pkgstate_cache_race")
    }
    data.frame(
               package = rows$package, version = rows$version,
               priority = rows$priority, origin = global$origin[idx],
               site = global$site[idx], suite = global$suite[idx],
               component = global$component[idx], installed = rows$installed,
               phased = rows$phased, verprio = rows$verprio,
               stringsAsFactors = FALSE
    )
}

## Parses `apt-cache policy` (no arguments): the "Package files:" section,
## one entry per source with its release fields. Returns a lookup table
## keyed by "uri dist arch" (or the status file path), with origin (o=),
## suite (a=), component (c=), and site (the origin host line).
parse_policy_global <- function(lines) {
    start <- match("Package files:", lines)
    if (is.na(start)) {
        stop_pkgstate("apt-cache policy output has no 'Package files:' section",
                   class = "runix_parse_error")
    }
    key <- origin <- suite <- component <- site <- character()
    n <- 0L
    for (i in seq(start + 1L, length(lines))) {
        line <- lines[i]
        if (line == "Pinned packages:" || grepl("^[^ ]", line)) {
            break
        }
        if (grepl("^ -?[0-9]+ ", line)) {
            tok <- strsplit(trimws(line), " +")[[1L]]
            n <- n + 1L
            key[n] <- if (length(tok) == 2L) {
                tok[2L]
            } else if (length(tok) == 5L && tok[5L] == "Packages") {
                paste(tok[2L], tok[3L], tok[4L])
            } else if (length(tok) == 4L && tok[4L] == "Packages") {
                ## exact-path repo (dist ends in "/"): no component, no arch
                paste(tok[2L], tok[3L])
            } else {
                stop_pkgstate("unparseable package-files entry (line ", i, "): ",
                           line, class = "runix_parse_error")
            }
            origin[n] <- suite[n] <- component[n] <- site[n] <- ""
        } else if (grepl("^ +release ", line) && n > 0L) {
            fields <- strsplit(
                               strsplit(sub("^ +release ", "", line), ",", fixed = TRUE)[[1L]],
                               "=", fixed = TRUE
            )
            for (f in fields) {
                if (length(f) == 2L) {
                    if (f[1L] == "o") {
                        origin[n] <- f[2L]
                    }
                    if (f[1L] == "a") {
                        suite[n] <- f[2L]
                    }
                    if (f[1L] == "c") {
                        component[n] <- f[2L]
                    }
                }
            }
        } else if (grepl("^ +origin ", line) && n > 0L) {
            site[n] <- sub("^ +origin ", "", line)
        } else if (nzchar(trimws(line))) {
            stop_pkgstate("unparseable line in package-files section (line ",
                       i, "): ", line, class = "runix_parse_error")
        }
    }
    data.frame(key = key, origin = origin, suite = suite,
               component = component, site = site, stringsAsFactors = FALSE)
}

## Parses `apt-cache policy pkg...` version tables into one row per
## (package, version, source line), keyed like parse_policy_global().
## Fail-closed: any unrecognized line is an error.
parse_policy_packages <- function(lines) {
    package <- version <- key <- character()
    priority <- phased <- verprio <- integer()
    installed <- logical()
    n <- 0L
    cur_pkg <- cur_ver <- NA_character_
    cur_inst <- FALSE
    cur_phased <- cur_verprio <- NA_integer_
    for (i in seq_along(lines)) {
        line <- lines[i]
        if (!nzchar(line)) {
            next
        }
        if (grepl("^[^ ].*:$", line)) {
            cur_pkg <- sub(":$", "", line)
            cur_ver <- NA_character_
        } else if (grepl("^  (Installed|Candidate|Version table|Package pin):",
                         line)) {
            next
        } else if (grepl("^ \\*\\*\\* ", line) || grepl("^     [^ ]", line)) {
            if (is.na(cur_pkg)) {
                stop_pkgstate("version line before any package header (line ", i,
                           ")", class = "runix_parse_error")
            }
            cur_inst <- grepl("^ \\*\\*\\* ", line)
            tok <- strsplit(trimws(sub("^ \\*\\*\\*", "", line)), " +")[[1L]]
            ## "VERSION PRIORITY", optionally annotated "(phased N%)" —
            ## the archive's Phased-Update-Percentage, captured verbatim.
            ok <- length(tok) == 2L ||
            (length(tok) == 4L && tok[3L] == "(phased" &&
                grepl("%\\)$", tok[4L]))
            if (!ok) {
                stop_pkgstate("unparseable version line (line ", i, "): ", line,
                           class = "runix_parse_error")
            }
            cur_ver <- tok[1L]
            ## effective (pin-altered) version priority, distinct from the
            ## per-source priorities on the lines below it
            cur_verprio <- as.integer(tok[2L])
            cur_phased <- if (length(tok) == 4L) {
                as.integer(sub("%\\)$", "", tok[4L]))
            } else {
                NA_integer_
            }
        } else if (grepl("^        -?[0-9]+ ", line)) {
            if (is.na(cur_pkg) || is.na(cur_ver)) {
                stop_pkgstate("source line before any version line (line ", i,
                           ")", class = "runix_parse_error")
            }
            tok <- strsplit(trimws(line), " +")[[1L]]
            n <- n + 1L
            key[n] <- if (length(tok) == 2L) {
                tok[2L]
            } else if (length(tok) == 5L && tok[5L] == "Packages") {
                paste(tok[2L], tok[3L], tok[4L])
            } else if (length(tok) == 4L && tok[4L] == "Packages") {
                ## exact-path repo (dist ends in "/"): no component, no arch
                paste(tok[2L], tok[3L])
            } else {
                stop_pkgstate("unparseable source line (line ", i, "): ", line,
                           class = "runix_parse_error")
            }
            package[n] <- cur_pkg
            version[n] <- cur_ver
            priority[n] <- as.integer(tok[1L])
            installed[n] <- cur_inst
            phased[n] <- cur_phased
            verprio[n] <- cur_verprio
        } else {
            stop_pkgstate("unparseable apt-cache policy line (line ", i, "): ",
                       line, class = "runix_parse_error")
        }
    }
    data.frame(package = package, version = version, priority = priority,
               key = key, installed = installed, phased = phased,
               verprio = verprio, stringsAsFactors = FALSE)
}
