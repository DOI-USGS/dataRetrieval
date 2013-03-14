\name{retrieveNWISqwData}
\alias{retrieveNWISqwData}
\title{Raw Data Import for USGS NWIS QW Data}
\usage{
  retrieveNWISqwData(siteNumber, pCodes, startDate,
    endDate)
}
\arguments{
  \item{siteNumber}{string or vector of strings USGS site
  number.  This is usually an 8 digit number}

  \item{pCodes}{string or vector of USGS parameter code.
  This is usually an 5 digit number.}

  \item{startDate}{string starting date for data retrieval
  in the form YYYY-MM-DD.}

  \item{endDate}{string ending date for data retrieval in
  the form YYYY-MM-DD.}
}
\value{
  data dataframe with agency, site, dateTime, value, and
  code columns
}
\description{
  Imports data from NWIS web service. This function gets
  the data from here:
  \url{http://nwis.waterdata.usgs.gov/nwis/qwdata} A list
  of parameter codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/} A list
  of statistic codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
}
\examples{
# These examples require an internet connection to run
siteNumber <- c('04024430','04024000')
startDate <- '2010-01-01'
endDate <- ''
pCodes <- c('34247','30234','32104','34220')
rawNWISqwData <- retrieveNWISqwData(siteNumber,pCodes,startDate,endDate)
# To get data in Sample dataframe format:
data <- rawNWISqwData[,names(rawNWISqwData) != "site"]
data$dateTime <- as.Date(data$dateTime)
compressedData <- compressData(data, interactive=interactive)
Sample <- populateSampleColumns(compressedData)
}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{USGS}
\keyword{web}

