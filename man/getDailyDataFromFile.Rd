\name{getDailyDataFromFile}
\alias{getDailyDataFromFile}
\title{Import Daily Data for WRTDS}
\usage{
getDailyDataFromFile(filePath, fileName, hasHeader = TRUE, separator = ",",
  qUnit = 1, interactive = TRUE)
}
\arguments{
  \item{filePath}{string specifying the path to the file}

  \item{fileName}{string name of file to open}

  \item{hasHeader}{logical true if the first row of data is
  the column headers}

  \item{separator}{string character that separates data
  cells}

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
Imports data from a user-supplied file, and converts it to
a Daily data frame, appropriate for WRTDS calculations.
}
\examples{
filePath <- system.file("extdata", package="dataRetrieval")
filePath <- paste(filePath,"/",sep="")
fileName <- "ChoptankRiverFlow.txt"
Daily <- getDailyDataFromFile(filePath,fileName,separator="\\t")
}
\keyword{USGS}
\keyword{WRTDS}
\keyword{data}
\keyword{file}
\keyword{import}

