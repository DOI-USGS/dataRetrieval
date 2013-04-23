\name{retrieveNWISData}
\alias{retrieveNWISData}
\title{Raw Data Import for USGS NWIS Data}
\usage{
  retrieveNWISData(siteNumber, ParameterCd, StartDate,
    EndDate, StatCd = "00003", format = "xml",
    interactive = TRUE)
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

  \item{StatCd}{string USGS statistic code. This is usually
  5 digits.  Daily mean (00003) is the default.}

  \item{format}{string, can be "tsv" or "xml", and is only
  applicable for daily and unit value requests.  "tsv"
  returns results faster, but there is a possiblitiy that
  an incomplete file is returned without warning. XML is
  slower, but will offer a warning if the file was
  incomplete (for example, if there was a momentary problem
  with the internet connection). It is possible to safely
  use the "tsv" option, but the user must carefully check
  the results to see if the data returns matches what is
  expected. The default is therefore "xml".}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  data dataframe with agency, site, dateTime, value, and
  code columns
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
# These examples require an internet connection to run
siteNumber <- '04085427'
startDate <- '2012-01-01'
endDate <- '2012-06-30'
pCode <- "00060"
rawDailyQ <- retrieveNWISData(siteNumber,pCode, startDate, endDate)
rawDailyTemperature <- retrieveNWISData(siteNumber,'00010', startDate, endDate, StatCd='00001',interactive=FALSE)
rawDailyTemperatureTSV <- retrieveNWISData(siteNumber,'00010', startDate, endDate, StatCd='00001',format="tsv",interactive=FALSE)
rawDailyQAndTempMeanMax <- retrieveNWISData(siteNumber,c('00010','00060'), startDate, endDate, StatCd=c('00001','00003'), interactive=FALSE)
}
\keyword{USGS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

