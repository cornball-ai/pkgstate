#' Installed packages with a different candidate version
#'
#' Returns one row per installed package whose apt candidate version
#' differs from the installed version — "candidate-available". Whether an
#' upgrade would actually be applied (phasing cohort membership, dpkg
#' holds, dependency holds) is deliberately not decided here; that is a
#' planning question for a later phase. \code{phased_percent} passes the
#' archive's Phased-Update-Percentage annotation through verbatim so
#' callers can reason about rollout state.
#'
#' @return A data.frame with columns \code{package} (queried spelling:
#'   name, or name:arch for non-native architectures), \code{installed},
#'   \code{candidate}, \code{origin}, \code{site}, \code{suite},
#'   \code{component} (the candidate version's best source — highest pin
#'   priority, first on ties), \code{security} (logical: any candidate
#'   source is an Ubuntu security pocket), and \code{phased_percent}
#'   (integer 0-100, NA when unannotated).
#' @examples
#' \dontrun{
#' up <- apt_upgradable()
#' up[up$security, c("package", "installed", "candidate")]
#' }
#' @export
apt_upgradable <- function() {
    inst <- dpkg_installed()
    inst <- inst[inst$status == "installed",, drop = FALSE]
    if (nrow(inst) == 0L) {
        return(upgradable_empty())
    }
    native <- native_arch()
    qn <- unique(ifelse(inst$architecture %in% c(native, "all"), inst$package,
                        paste0(inst$package, ":", inst$architecture)))

    outputs <- run_policy_chunks(qn)
    cand <- do.call(rbind, c(lapply(outputs, parse_policy_candidates),
                             list(make.row.names = FALSE)))
    upg <- cand[!is.na(cand$candidate) & !is.na(cand$installed) &
        cand$candidate != cand$installed,, drop = FALSE]
    if (nrow(upg) == 0L) {
        return(upgradable_empty())
    }

    rows <- joined_policy_rows(outputs)
    out <- lapply(seq_len(nrow(upg)), function(j) {
        name <- upg$package[j]
        cv <- upg$candidate[j]
        r <- rows[rows$package == name & rows$version == cv &
            rows$suite != "now",, drop = FALSE]
        if (nrow(r) == 0L) {
            stop_pkgstate("candidate version ", cv, " of ", name,
                       " has no archive source in policy output; the apt cache ",
                       "may have changed between queries - retry",
                       class = "pkgstate_cache_race")
        }
        best <- r[order(-r$priority)[1L],]
        data.frame(
                   package = name, installed = upg$installed[j], candidate = cv,
                   origin = best$origin, site = best$site, suite = best$suite,
                   component = best$component,
                   security = any(r$origin == "Ubuntu" & grepl("-security$", r$suite)),
                   phased_percent = r$phased[1L],
                   stringsAsFactors = FALSE
        )
    })
    do.call(rbind, c(out, list(make.row.names = FALSE)))
}

upgradable_empty <- function() {
    data.frame(package = character(), installed = character(),
               candidate = character(), origin = character(),
               site = character(), suite = character(),
               component = character(), security = logical(),
               phased_percent = integer(), stringsAsFactors = FALSE)
}
