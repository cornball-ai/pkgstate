% tinyrox says don't edit this manually, but it can't stop you!
\name{apt_upgradable}
\alias{apt_upgradable}
\title{Installed packages with a different candidate version}
\usage{
apt_upgradable()
}
\value{
A data.frame with columns \code{package} (queried spelling:
  name, or name:arch for non-native architectures), \code{installed},
  \code{candidate}, \code{origin}, \code{site}, \code{suite},
  \code{component} (the candidate version's best source — highest pin
  priority, first on ties), \code{security} (logical: any candidate
  source is an Ubuntu security pocket), and \code{phased_percent}
  (integer 0-100, NA when unannotated).
}
\description{
Returns one row per installed package whose apt candidate version
differs from the installed version — "candidate-available". Whether an
upgrade would actually be applied (phasing cohort membership, dpkg
holds, dependency holds) is deliberately not decided here; that is a
planning question for a later phase. \code{phased_percent} passes the
archive's Phased-Update-Percentage annotation through verbatim so
callers can reason about rollout state.
}
\examples{
\dontrun{
up <- apt_upgradable()
up[up$security, c("package", "installed", "candidate")]
}
}
