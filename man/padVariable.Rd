\name{padVariable}
\alias{padVariable}
\title{Pad string with leading zeros}
\usage{
  padVariable(x, padTo)
}
\arguments{
  \item{x}{string}

  \item{padTo}{number Final desired length of the string}
}
\value{
  x string returned with leading zeros
}
\description{
  Function to pad a string with leading zeros. Useful for
  parameter codes and USGS site IDs.
}
\examples{
pCode <- '10'
correctPCode <- padVariable(pCode,5)
}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{USGS}
\keyword{web}

