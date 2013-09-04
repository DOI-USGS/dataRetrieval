\name{formatCheckSiteNumber}
\alias{formatCheckSiteNumber}
\title{formatCheckSiteNumber}
\usage{
  formatCheckSiteNumber(siteNumber, interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string to check}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  siteNumber string
}
\description{
  Checks that the site code is 8 digits. If it is not 8
  digits, it confirms with the user.
}
\examples{
site<- '01234567'
formatCheckSiteNumber(site)
}
\keyword{WRTDS}
\keyword{flow}

