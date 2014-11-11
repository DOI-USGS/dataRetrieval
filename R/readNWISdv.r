#' Raw Data Import for USGS NWIS Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://help.waterdata.usgs.gov/codes-and-parameters/parameters}
#' A list of statistic codes can be found here: \url{http://help.waterdata.usgs.gov/code/stat_code_query?fmt=html}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number. Multiple sites can be requested with a string vector.
#' @param parameterCd string or vector of USGS parameter code.  This is usually an 5 digit number..
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param statCd string USGS statistic code. This is usually 5 digits.  Daily mean (00003) is the default.
#' @return data dataframe with agency, site, dateTime, value, and code columns
#' @export
#' @keywords data import USGS web service
#' @examples
#' siteNumber <- '04085427'
#' startDate <- '2012-01-01'
#' endDate <- '2012-06-30'
#' pCode <- '00060'
#' rawDailyQ <- readNWISdv(siteNumber,pCode, startDate, endDate)
#' rawDailyQAndTempMeanMax <- readNWISdv(siteNumber,c('00010','00060'),
#'        startDate, endDate, statCd=c('00001','00003'))
#' rawDailyQAndTempMeanMax <- renameNWISColumns(rawDailyQAndTempMeanMax)
#' rawDailyMultiSites<- readNWISdv(c("01491000","01645000"),c('00010','00060'),
#'        startDate, endDate, statCd=c('00001','00003'))
#' # Site with no data:
#' x <- readNWISdv("10258500","00060", "2014-09-08", "2014-09-14")
#' names(attributes(x))
#' attr(x, "siteInfo")
#' attr(x, "variableInfo")
readNWISdv <- function (siteNumber,parameterCd,startDate="",endDate="",statCd="00003"){  
  
  url <- constructNWISURL(siteNumber,parameterCd,startDate,endDate,"dv",statCd=statCd)

  data <- importWaterML1(url, asDateTime=FALSE)
  data$dateTime <- as.Date(data$dateTime)

  return (data)
}
