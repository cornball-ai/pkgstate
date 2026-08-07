% tinyrox says don't edit this manually, but it can't stop you!
\name{apt_origins}
\alias{apt_origins}
\title{Origins of available package versions}
\usage{
apt_origins(packages)
}
\arguments{
\item{packages}{Character vector of package names, length >= 1. The
all-known-packages form is reserved for a future native libapt backend.}
}
\value{
A data.frame with columns \code{package}, \code{version},
  \code{priority}, \code{origin}, \code{site}, \code{suite},
  \code{component}, and \code{installed}. The dpkg status pseudo-source
  appears with \code{origin ""}, \code{site ""}, \code{suite "now"}.
  \code{installed} marks the currently installed version. Unknown
  package names yield zero rows and are not an error.
}
\description{
For each requested package, returns one row per (version, source) from
apt's policy tables: which archive each available version comes from,
with apt's Origin label, suite, and component. This is the raw material
for origin classification (main/universe/ESM/PPA/third-party), which is
deliberately left to callers.
}
\examples{
\dontrun{
o <- apt_origins(c("dpkg", "bash"))
o[o$suite != "now", c("package", "version", "origin", "component")]
}
}
