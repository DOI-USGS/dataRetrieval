\name{getQWDataFromFile}
\alias{getQWDataFromFile}
\title{Basic Data Import}
\usage{
  getQWDataFromFile(filePath, fileName, hasHeader = TRUE,
    separator = ",")
}
\arguments{
  \item{filePath}{string specifying the path to the file}

  \item{fileName}{string name of file to open}

  \item{hasHeader}{logical true if the first row of data is
  the column headers}

  \item{separator}{string character that separates data
  cells}
}
\value{
  retval dataframe
}
\description{
  Imports data from user-supplied data file. Specifically
  used to import water quality data for use in the WRTDS
  package. For WRTDS usage, the first column is expected to
  be dates, the second column remarks (specifically < if
  censored data), and the third column is measured values.
  There can be additional columns of data, for each column
  of data, there should be a remark column preceeding.
}
\examples{
# Examples of how to use getQWDataFromFile:
# Change the file path and file name to something meaningful:
filePath <- '~/RData/'  # Sample format
fileName <- 'ChoptankRiverNitrate.csv'
\dontrun{rawSampleData <- getQWDataFromFile(filePath,fileName, separator=";")}
}
\keyword{data}
\keyword{file}
\keyword{import}

