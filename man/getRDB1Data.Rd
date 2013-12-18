\name{getRDB1Data}
\alias{getRDB1Data}
\title{Function to return data from the NWIS RDB 1.0 format}
\usage{
getRDB1Data(obs_url, asDateTime = FALSE)
}
\arguments{
  \item{obs_url}{string containing the url for the
  retrieval}

  \item{asDateTime}{logical, if TRUE returns date and time
  as POSIXct, if FALSE, Date}
}
\value{
data a data frame containing columns agency, site,
dateTime, values, and remark codes for all requested
combinations
}
\description{
This function accepts a url parameter that already contains
the desired NWIS site, parameter code, statistic, startdate
and enddate.
}
\examples{
sites <- "02177000"
startDate <- "2012-09-01"
endDate <- "2012-10-01"
offering <- '00003'
property <- '00060'
obs_url <- constructNWISURL(sites,property,startDate,endDate,'dv',format='tsv')
data <- getRDB1Data(obs_url)
urlMulti <- constructNWISURL("04085427",c("00060","00010"),startDate,endDate,'dv',statCd=c("00003","00001"),'tsv')
multiData <- getRDB1Data(urlMulti)
}

