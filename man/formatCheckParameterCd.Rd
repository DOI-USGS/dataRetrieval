\name{formatCheckParameterCd}
\alias{formatCheckParameterCd}
\title{formatCheckParameterCd}
\usage{
formatCheckParameterCd(ParameterCd, interactive = TRUE)
}
\arguments{
  \item{ParameterCd}{string to check}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
ParameterCd string
}
\description{
Checks that the parameter code is 5 digits. If it is less,
it will pad the string with zeros. If more, ask the user to
re-enter.
}
\examples{
pCode <- '01234'
formatCheckParameterCd(pCode)
}
\keyword{WRTDS}
\keyword{flow}

