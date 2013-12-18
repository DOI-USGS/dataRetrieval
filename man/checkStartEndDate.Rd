\name{checkStartEndDate}
\alias{checkStartEndDate}
\title{checkStartEndDate}
\usage{
checkStartEndDate(StartDate, EndDate, interactive = TRUE)
}
\arguments{
  \item{StartDate}{string}

  \item{EndDate}{string}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
vector where first value is StartDate, second is EndDate
}
\description{
Checks that the start date is before the end date.  If not,
it will give the user the opportunity to correct, otherwise
will create a warning.
}
\examples{
startDate <- '1985-01-01'
endDate <- '1990-01-01'
checkStartEndDate(startDate, endDate)
}
\keyword{WRTDS}
\keyword{flow}

