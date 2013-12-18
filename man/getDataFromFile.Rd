\name{getDataFromFile}
\alias{getDataFromFile}
\title{Basic Data Import for Water Flow Data}
\usage{
getDataFromFile(filePath, fileName, hasHeader = TRUE, separator = ",")
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
retval dataframe with dateTime, value, and code columns
}
\description{
Imports data from user-supplied data file. Specifically
used to import water flow data for use in the WRTDS
package. For WRTDS usage, the first column is expected to
be dates, the second column measured values. The third
column is optional, it contains any remark codes.
}
\examples{
# Examples of how to use getDataFromFile:
# Change the file path and file name to something meaningful:
filePath <- '~/RData/'  # Sample format
fileName <- 'ChoptankRiverFlow.txt'
\dontrun{getDataFromFile(filePath,fileName, separator="\\t")}
}
\keyword{data}
\keyword{file}
\keyword{import}

