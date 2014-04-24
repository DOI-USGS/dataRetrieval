\name{getDataAvailability}
\alias{getDataAvailability}
\title{USGS data availability}
\usage{
getDataAvailability(siteNumber, interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string USGS site number.  This is
  usually an 8 digit number}

  \item{interactive}{logical Option for interactive mode.
  If true, a progress indicator is printed to the console.}
}
\value{
retval dataframe with all information found in the expanded
site file
}
\description{
Imports a table of available parameters, period of record,
and count. There is also an option to load the long
parameter names and additional information on the
parameters with longNames=TRUE.
}
\examples{
# These examples require an internet connection to run
availableData <- getDataAvailability('05114000')
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

