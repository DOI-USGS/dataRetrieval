\name{checkStartEndDate}
\alias{checkStartEndDate}
\title{Check Start End Dates}
\usage{
  checkStartEndDate(StartDate, EndDate, interactive = TRUE)
}
\arguments{
  \item{StartDate}{string date}

  \item{EndDate}{string date}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  list StartDate,EndDate
}
\description{
  Checks that the user-supplied starting date is before the
  ending date.  If not, either the user can re-enter, or
  the dates will be set to maximum.
}
\examples{
checkStartEndDate('1985-01-01', '1985-12-31')
}
\keyword{data}
\keyword{from}
\keyword{import}
\keyword{service}
\keyword{web}

