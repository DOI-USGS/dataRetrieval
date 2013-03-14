\name{getPreLoadedData}
\alias{getPreLoadedData}
\title{Convert Preloaded Flow Data}
\usage{
  getPreLoadedData(rawData)
}
\arguments{
  \item{rawData}{dataframe data already loaded into R
  workspace}
}
\value{
  retval dataframe with dateTime, value, and code columns
}
\description{
  Imports data from data already pre-loaded into the R
  workspace. Specifically used to import water flow data
  for use in the WRTDS package. For WRTDS usage, the first
  column is expected to be dates, the second column
  measured values. The third column is optional, it
  contains any remark codes.
}
\examples{
ChoptankRiverFlow <- ChoptankRiverFlow
rawData <- getPreLoadedData(ChoptankRiverFlow)
rawDataSample <- getPreLoadedData(ChoptankRiverNitrate)
}
\keyword{data}
\keyword{import}

