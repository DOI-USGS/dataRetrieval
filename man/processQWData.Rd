\name{processQWData}
\alias{processQWData}
\title{Processing of USGS NWIS Water Quality Data}
\usage{
processQWData(data)
}
\arguments{
  \item{data}{dataframe from}
}
\value{
data dataframe with first column dateTime, and at least one
qualifier and value columns (subsequent qualifier/value
columns could follow depending on the number of parameter
codes)
}
\description{
Processes water quality portal data. This function looks at
detection limit and detection conditions to determine if a
value is left censored or not. Censored values are given
the qualifier "<".  The dataframe is also converted from a
long to wide format.
}
\examples{
# These examples require an internet connection to run
rawSample <- getRawQWData('01594440','01075', '1985-01-01', '1985-03-31')
rawSampleSelect <- processQWData(rawSample)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

