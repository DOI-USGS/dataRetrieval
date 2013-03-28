#' Raw Data Import for USGS NWIS Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param ParameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param StartDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param EndDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param StatCd string USGS statistic code. This is usually 5 digits.  Daily mean (00003) is the default.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return data dataframe with agency, site, dateTime, value, and code columns
#' @export
#' @examples
#' # These examples require an internet connection to run
#' siteNumber <- '04085427'
#' startDate <- '2012-01-01'
#' endDate <- '2012-06-30'
#' pCode <- "00060"
#' rawDailyQ <- retrieveNWISData(siteNumber,pCode, startDate, endDate)
#' rawDailyTemperature <- retrieveNWISData(siteNumber,'00010', startDate, endDate, StatCd='00001',interactive=FALSE)
#' rawDailyQAndTempMeanMax <- retrieveNWISData(siteNumber,c('00010','00060'), startDate, endDate, StatCd=c('00001','00003'), interactive=FALSE)
retrieveNWISData <- function (siteNumber,ParameterCd,StartDate,EndDate,StatCd="00003",interactive=TRUE){  
  
  url <- constructNWISURL(siteNumber,ParameterCd,StartDate,EndDate,"dv",StatCd)
  data <- getWaterML1Data(url)

  return (data)
}