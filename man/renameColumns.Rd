\name{renameColumns}
\alias{renameColumns}
\title{renameColumns}
\usage{
renameColumns(rawData)
}
\arguments{
  \item{rawData}{dataframe returned from retrieval
  functions}
}
\value{
rawData dataframe with improved column names
}
\description{
Rename columns coming back from NWIS data retrievals
}
\examples{
# This example requires an internet connection to run
siteNumber <- '05114000'
rawData <- retrieveNWISData(siteNumber,c("00010","00060","00300"),"2001-01-01","2002-01-01",StatCd=c("00001","00003"))
rawData <- renameColumns(rawData)
today <- as.character(Sys.Date())
rawData2 <- retrieveUnitNWISData(siteNumber,c("00010","00060"),today,today)
rawData2 <- renameColumns(rawData2)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

