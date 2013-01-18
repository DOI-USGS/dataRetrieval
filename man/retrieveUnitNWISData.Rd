\name{retrieveUnitNWISData}
\alias{retrieveUnitNWISData}
\title{Raw Data Import for Instantaneous USGS NWIS Data}
\usage{
  retrieveUnitNWISData(siteNumber, ParameterCd, StartDate,
    EndDate, interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string USGS site number.  This is
  usually an 8 digit number}

  \item{ParameterCd}{string USGS parameter code.  This is
  usually an 5 digit number.}

  \item{StartDate}{string starting date for data retrieval
  in the form YYYY-MM-DD.}

  \item{EndDate}{string ending date for data retrieval in
  the form YYYY-MM-DD.}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  retval dataframe with agency, site, dateTime, time zone,
  value, and code columns
}
\description{
  Imports data from NWIS web service. This function gets
  the data from here: \url{http://waterservices.usgs.gov/}
  A list of parameter codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/} A list
  of statistic codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
}
\examples{
siteNumber <- '05114000'
ParameterCd <- '00060'
StartDate <- '2012-05-01'
EndDate <- '2012-05-02'
# These examples require an internet connection to run
rawData <- retrieveUnitNWISData(siteNumber,ParameterCd,StartDate,EndDate,interactive=FALSE)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

