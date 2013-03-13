\name{constructNWISURL}
\alias{constructNWISURL}
\title{Construct NWIS url for data retrieval}
\usage{
  constructNWISURL(siteNumber, parameterCd, startDate,
    endDate, service, statCd = "00003")
}
\arguments{
  \item{siteNumber}{string or vector of strings USGS site
  number.  This is usually an 8 digit number}

  \item{parameterCd}{string or vector of USGS parameter
  code.  This is usually an 5 digit number.}

  \item{startDate}{string starting date for data retrieval
  in the form YYYY-MM-DD.}

  \item{endDate}{string ending date for data retrieval in
  the form YYYY-MM-DD.}

  \item{statCd}{string USGS statistic code only used for
  daily value service. This is usually 5 digits.  Daily
  mean (00003) is the default.}

  \item{service}{string USGS service to call. Possible
  values are "dv" (daily values), "uv" (unit/instantaneous
  values), "qw" (water quality data), and "wqp" (water
  quality portal, which can include STORET).}
}
\value{
  url string
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
siteNumber <- '04085427'
startDate <- '2012-01-01'
endDate <- '2012-06-30'
pCode <- c("00060","00010")
url_daily <- constructNWISURL(siteNumber,pCode,startDate,endDate,'dv')
url_unit <- constructNWISURL(siteNumber,pCode,startDate,endDate,'iv')
url_qw_single <- constructNWISURL(siteNumber,"34220",startDate,endDate,'qwdata')
url_qw <- constructNWISURL(siteNumber,c('34247','30234','32104','34220'),startDate,endDate,'qwdata')
url_wqp <- constructNWISURL(siteNumber,"34220",startDate,endDate,'wqp')
}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{USGS}
\keyword{web}

