\name{formatCheckDate}
\alias{formatCheckDate}
\title{Date Formatting Correction}
\usage{
  formatCheckDate(Date, dateString, interactive = TRUE)
}
\arguments{
  \item{Date}{string date to check}

  \item{dateString}{string should be either 'StartDate' or
  'EndDate'}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  Date string
}
\description{
  Checks that the user-supplied date is in the format
  YYYY-MM-DD. If not, asks the user to re-enter.
}
\examples{
formatCheckDate('1985-01-01', 'StartDate')
}
\keyword{data}
\keyword{from}
\keyword{import}
\keyword{service}
\keyword{web}

