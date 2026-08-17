% tinyrox says don't edit this manually, but it can't stop you!
\name{dpkg_selections}
\alias{dpkg_selections}
\title{dpkg selection (want) state}
\usage{
dpkg_selections(packages = NULL)
}
\arguments{
\item{packages}{Optional character vector of package names to filter to;
\code{NULL} (default) returns every entry dpkg knows. Filtering is applied
in R after the full read, so an unknown name simply yields no row rather
than an error.}
}
\value{
A data.frame with columns \code{package}, \code{architecture}, and
  \code{selection}. \code{selection} is dpkg's want word, verbatim:
  \code{"install"}, \code{"hold"}, \code{"deinstall"}, \code{"purge"}, or
  \code{"unknown"}.
}
\description{
Reads the dpkg selection -- the \dQuote{want} field of each package's status
triplet -- via \code{dpkg-query}. This is the hold state: a held package has
selection \code{"hold"}, a normally-installed one \code{"install"}. It is
distinct from \code{\link{dpkg_installed}}'s status word (\code{installed},
\code{config-files}), which is the current \emph{state}, not the desired
selection.
}
\examples{
\dontrun{
sel <- dpkg_selections()
sel[sel$selection == "hold", ]
}
}
