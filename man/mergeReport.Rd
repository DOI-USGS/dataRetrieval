\name{mergeReport}
\alias{mergeReport}
\title{Merge Sample and Daily Data for WRTDS}
\usage{
mergeReport(localDaily = Daily, localSample = Sample, interactive = TRUE)
}
\arguments{
  \item{localDaily}{dataframe containing the daily data,
  default is Daily}

  \item{localSample}{dataframe containing the sample data,
  default is Sample}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
newSample dataframe with merged flow information
}
\description{
Merges the flow data from the daily record into the sample
record.
}
\examples{
# These examples require an internet connection to run
Daily <- getDVData('01594440','00060', '1985-01-01', '1985-03-31')
Sample <- getSampleData('01594440','01075', '1985-01-01', '1985-03-31')
Sample <- mergeReport()
}
\seealso{
\code{\link{getDVData}},
\code{\link{populateSampleColumns}}
}
\keyword{USGS}
\keyword{WRTDS}
\keyword{data}
\keyword{import}

