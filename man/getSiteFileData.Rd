\name{getSiteFileData}
\alias{getSiteFileData}
\title{USGS Site File Data Retrieval}
\usage{
  getSiteFileData(siteNumber = "", interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string USGS site number.  This is
  usually an 8 digit number}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  retval dataframe with all information found in the
  expanded site file
}
\description{
  Imports data from USGS site file site. This function gets
  data from here: \url{http://waterservices.usgs.gov/}
}
\examples{
# These examples require an internet connection to run
siteINFO <- getSiteFileData('05114000',interactive=FALSE)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

