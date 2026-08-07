% tinyrox says don't edit this manually, but it can't stop you!
\name{apt_cache_timestamps}
\alias{apt_cache_timestamps}
\title{Timestamps of apt list and dpkg status state}
\usage{
apt_cache_timestamps(
  lists_dir = "/var/lib/apt/lists",
  status_file = "/var/lib/dpkg/status"
)
}
\arguments{
\item{lists_dir}{Directory of apt index stamps, default
\code{/var/lib/apt/lists}.}

\item{status_file}{The dpkg status database, default
\code{/var/lib/dpkg/status}.}
}
\value{
A list with \code{lists_updated} (POSIXct UTC; NA when the
  directory holds no index stamps — never updated) and
  \code{status_changed} (POSIXct UTC). A missing path is an error,
  never NA.
}
\description{
Read-only status query: when the apt package lists were last refreshed,
and when the dpkg status database last changed. Nothing here refreshes
anything.
}
\examples{
\dontrun{
apt_cache_timestamps()
}
}
