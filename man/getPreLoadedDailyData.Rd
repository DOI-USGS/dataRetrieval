\name{getPreLoadedDailyData}
\alias{getPreLoadedDailyData}
\title{Import Preloaded Daily Data for WRTDS use}
\usage{
  getPreLoadedDailyData(loadedData, qUnit = 1,
    interactive = TRUE)
}
\arguments{
  \item{loadedData}{dataframe that is already loaded in R
  session}

  \item{qUnit}{number 1 is cubic feet per second, 2 is
  cubic meters per second, 3 is 10^3 cubic feet per second,
  and 4 is 10^3 cubic meters per second}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  Daily dataframe
}
\description{
  This function is intended to convert daily data that is
  already loaded into the R enviornment to the WRTDS daily
  data frame.
}
\examples{
Daily <- getPreLoadedDailyData(ChoptankRiverFlow, interactive=FALSE)
}
\keyword{data}
\keyword{import}
\keyword{WRTDS}

