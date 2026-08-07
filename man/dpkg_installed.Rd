% tinyrox says don't edit this manually, but it can't stop you!
\name{dpkg_installed}
\alias{dpkg_installed}
\title{Installed packages from the dpkg database}
\usage{
dpkg_installed()
}
\value{
A data.frame with columns \code{package}, \code{version},
  \code{architecture}, and \code{status}. \code{status} is dpkg's status
  word, verbatim (e.g. \code{"installed"}, \code{"config-files"}).
}
\description{
Queries the dpkg status database and returns one row per package entry
known to dpkg, including entries in states other than fully installed
(for example \code{config-files} left by removed-but-not-purged packages).
}
\examples{
\dontrun{
pkgs <- dpkg_installed()
pkgs[pkgs$status != "installed", ]
}
}
