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
# These examples require an internet connection to run
siteNumber <- '05114000'
ParameterCd <- c('00060','00065')
StartDate <- as.character(Sys.Date())
EndDate <- as.character(Sys.Date())
# These examples require an internet connection to run
rawData <- retrieveUnitNWISData(siteNumber,ParameterCd,StartDate,EndDate,interactive=FALSE)
rawData <- renameColumns(rawData)
rawData2 <- retrieveNWISData(siteNumber,c("00010","00060","00300"),"2001-01-01","2002-01-01",StatCd=c("00001","00003"),interactive=FALSE)
rawData2 <- renameColumns(rawData2)
site <- '04027000'
pCodes <- c("00010","00060","00095","00300","00400","63680")
rawData3 <- retrieveUnitNWISData(site,pCodes,StartDate,EndDate,interactive=FALSE)
rawData3 <- renameColumns(rawData3)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

