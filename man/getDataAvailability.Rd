\name{getDataAvailability}
\alias{getDataAvailability}
\title{USGS data availability}
\usage{
  getDataAvailability(siteNumber = "", interactive = TRUE,
    longNames = FALSE)
}
\arguments{
  \item{siteNumber}{string USGS site number.  This is
  usually an 8 digit number}

  \item{interactive}{logical Option for interactive mode.
  If true, a progress indicator is printed to the console.}

  \item{longNames}{logical indicates whether or not to make
  a web call to get long names of parameters. Be aware this
  could take a very long time if the station has lots of
  data.}
}
\value{
  retval dataframe with all information found in the
  expanded site file
}
\description{
  Imports a table of available parameters, period of
  record, and count. There is also an option to load the
  long parameter names and additional information on the
  parameters with longNames=TRUE.
}
\examples{
# These examples require an internet connection to run
availableData <- getDataAvailability('05114000',interactive=FALSE)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

