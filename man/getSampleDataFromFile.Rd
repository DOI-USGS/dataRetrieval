\name{getSampleDataFromFile}
\alias{getSampleDataFromFile}
\title{Import Sample Data for WRTDS}
\usage{
getSampleDataFromFile(filePath, fileName, hasHeader = TRUE, separator = ",",
  interactive = TRUE)
}
\arguments{
  \item{filePath}{string specifying the path to the file}

  \item{fileName}{string name of file to open}

  \item{hasHeader}{logical true if the first row of data is
  the column headers}

  \item{separator}{string character that separates data
  cells}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
Sample dataframe
}
\description{
Imports data from a user-supplied file, and converts it to
a Sample data frame (including summing multiple
constituents), appropriate for WRTDS calculations. See
section 3.4 of the vignette for more details.
}
\examples{
# Examples of how to use getSampleDataFromFile:
# Change the file path and file name to something meaningful:
filePath <- '~/RData/'  # Sample format
fileName <- 'ChoptankRiverNitrate.csv'
\dontrun{Sample <- getSampleDataFromFile(filePath,fileName, separator=";",interactive=FALSE)}
}
\keyword{USGS}
\keyword{WRTDS}
\keyword{data}
\keyword{file}
\keyword{import}

