% tinyrox says don't edit this manually, but it can't stop you!
\name{apt_policy}
\alias{apt_policy}
\title{Full apt policy view for one package}
\usage{
apt_policy(package)
}
\arguments{
\item{package}{A single package name (name, or name:arch).}
}
\value{
A list with elements \code{package}, \code{installed},
  \code{candidate} (versions, NA when \code{(none)}), \code{pin} (the
  "Package pin:" version, NA when unpinned), and \code{versions} — a
  data.frame with columns \code{version}, \code{version_priority}
  (effective), \code{priority} (source), \code{origin}, \code{site},
  \code{suite}, \code{component}, \code{installed}.
}
\description{
The single-package diagnostic view: where the bulk views report data
(\code{\link{apt_origins}}: per-source rows; \code{\link{apt_candidates}}:
resolution results), \code{apt_policy()} explains why resolution went the
way it did — the package pin when present, and each version's effective
priority (the number pins alter, which can be negative), alongside its
sources.
}
\examples{
\dontrun{
p <- apt_policy("dpkg")
p$versions
}
}
