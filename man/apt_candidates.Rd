% tinyrox says don't edit this manually, but it can't stop you!
\name{apt_candidates}
\alias{apt_candidates}
\title{Installed and candidate versions of packages}
\usage{
apt_candidates(packages)
}
\arguments{
\item{packages}{Character vector of package names, length >= 1. The
all-known-packages form is reserved for a future native libapt backend.}
}
\value{
A data.frame with columns \code{package}, \code{installed}, and
  \code{candidate}. Version columns are \code{NA} where apt reports
  \code{(none)}. Unknown package names yield zero rows and are not an
  error.
}
\description{
For each requested package, returns dpkg's installed version and apt's
candidate version (the version an upgrade would install), as reported by
the policy engine.
}
\examples{
\dontrun{
apt_candidates(c("dpkg", "bash"))
}
}
