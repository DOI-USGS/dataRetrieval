\name{getWaterML1Data}
\alias{getWaterML1Data}
\title{Function to return data from the NWISWeb WaterML1.1 daily values service}
\usage{
  getWaterML1Data(obs_url)
}
\arguments{
  \item{obs_url}{string containing the url for the
  retrieval}
}
\value{
  mergedDF a data frame containing columns agency, site,
  dateTime, values, and remark codes for all requested
  combinations
}
\description{
  This function accepts a url parameter that already
  contains the desired NWIS site, parameter code,
  statistic, startdate and enddate.
}
\examples{
url <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,1.1&sites="
sites <- "02177000"
startDate <- "2012-09-01"
endDate <- "2012-10-01"
offering <- '00003'
property <- '00060'
obs_url <- constructNWISURL(sites,property,startDate,endDate,'dv')
data <- getWaterML1Data(obs_url)
urlMulti <- constructNWISURL("04085427",c("00060","00010"),startDate,endDate,'dv',statCd=c("00003","00001"))
multiData <- getWaterML1Data(urlMulti)
}

