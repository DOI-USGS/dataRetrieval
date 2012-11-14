\name{formatCheckSiteNumber}
\alias{formatCheckSiteNumber}
\title{Site number check}
\usage{
  formatCheckSiteNumber(siteNumber, interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string USGS site number}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  siteNumber string
}
\description{
  Checks that the user-supplied site number is 8 digits,
  typical for many USGS station site id's.If not, asks the
  user to re-enter. The final siteNumber can be more or
  less than 8 digits, since there are conditions where that
  is the case.
}
\examples{
formatCheckSiteNumber('01594440')
formatCheckSiteNumber('015944400',interactive=FALSE)
}
\keyword{checking}
\keyword{data}
\keyword{error}
\keyword{for}
\keyword{from}
\keyword{import}
\keyword{service}
\keyword{web}

