\name{dateFormatCheck}
\alias{dateFormatCheck}
\title{Date Formatting Check}
\usage{
  dateFormatCheck(date)
}
\arguments{
  \item{date}{string date to check}
}
\value{
  Date logical
}
\description{
  Checks that the user-supplied date is in the format
  YYYY-MM-DD and month is less than 13, and dates are less
  than 32.
}
\examples{
dateFormatCheck('1985-01-01')
dateFormatCheck('01/01/1985')
}
\keyword{data}
\keyword{from}
\keyword{import}
\keyword{service}
\keyword{web}

