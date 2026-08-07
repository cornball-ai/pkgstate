#' Timestamps of apt list and dpkg status state
#'
#' Read-only status query: when the apt package lists were last refreshed,
#' and when the dpkg status database last changed. Nothing here refreshes
#' anything.
#'
#' @param lists_dir Directory of apt index stamps, default
#'   \code{/var/lib/apt/lists}.
#' @param status_file The dpkg status database, default
#'   \code{/var/lib/dpkg/status}.
#' @return A list with \code{lists_updated} (POSIXct UTC; NA when the
#'   directory holds no index stamps — never updated) and
#'   \code{status_changed} (POSIXct UTC). A missing path is an error,
#'   never NA.
#' @examples
#' \dontrun{
#' apt_cache_timestamps()
#' }
#' @export
apt_cache_timestamps <- function(lists_dir = "/var/lib/apt/lists",
                                 status_file = "/var/lib/dpkg/status") {
    if (!dir.exists(lists_dir)) {
        stop_rdpkg("apt lists directory not found: ", lists_dir)
    }
    if (!file.exists(status_file)) {
        stop_rdpkg("dpkg status file not found: ", status_file)
    }
    stamps <- list.files(lists_dir, full.names = TRUE)
    stamps <- stamps[!dir.exists(stamps) & basename(stamps) != "lock"]
    lists_updated <- if (length(stamps) == 0L) {
        as.POSIXct(NA_character_, tz = "UTC")
    } else {
        max(file.info(stamps)$mtime)
    }
    status_changed <- file.info(status_file)$mtime
    attr(lists_updated, "tzone") <- "UTC"
    attr(status_changed, "tzone") <- "UTC"
    list(lists_updated = lists_updated, status_changed = status_changed)
}
