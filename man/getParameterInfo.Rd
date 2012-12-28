\name{getParameterInfo}
\alias{getParameterInfo}
\title{USGS Parameter Data Retrieval}
\usage{
  getParameterInfo(parameterCd, interactive = TRUE)
}
\arguments{
  \item{parameterCd}{string USGS parameter code.  This is
  usually an 5 digit number.}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  parameterData dataframe with all information from the
  USGS about the particular parameter (usually code, name,
  short name, units, and CAS registry numbers)
}
\description{
  Imports data from NWIS about meaured parameter based on
  user-supplied parameter code. This function gets the data
  from here:
  \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes}
}
\examples{
# These examples require an internet connection to run
paramINFO <- getParameterInfo('01075')
paramINFO2 <- getParameterInfo('00931',interactive=FALSE)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

