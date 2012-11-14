\name{formatCheckParameterCd}
\alias{formatCheckParameterCd}
\title{Parameter Code Check}
\usage{
  formatCheckParameterCd(ParameterCd, interactive = TRUE)
}
\arguments{
  \item{ParameterCd}{string USGS parameter code}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  ParameterCd string
}
\description{
  Checks that the user-supplied parameter code is 5 digits.
  If not, asks the user to re-enter.
}
\examples{
formatCheckParameterCd('00060')
}
\keyword{data}
\keyword{from}
\keyword{import}
\keyword{service}
\keyword{web}

