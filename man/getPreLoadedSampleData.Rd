\name{getPreLoadedSampleData}
\alias{getPreLoadedSampleData}
\title{Import Preloaded Sample Data for WRTDS use}
\usage{
  getPreLoadedSampleData(loadedData, interactive = TRUE)
}
\arguments{
  \item{loadedData}{dataframe that is already loaded in R
  session}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  Sample dataframe
}
\description{
  This function is intended to convert sample data that is
  already loaded into the R enviornment to the WRTDS daily
  data frame.
}
\examples{
getPreLoadedSampleData(ChoptankRiverNitrate, interactive=FALSE)
}
\keyword{WRTDS}
\keyword{data}
\keyword{import}

