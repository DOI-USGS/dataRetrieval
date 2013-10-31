#' Raw Data Import for USGS NWIS Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://help.waterdata.usgs.gov/codes-and-parameters/parameters}
#' A list of statistic codes can be found here: \url{http://help.waterdata.usgs.gov/code/stat_code_query?fmt=html}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param ParameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param StartDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param EndDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param StatCd string USGS statistic code. This is usually 5 digits.  Daily mean (00003) is the default.
#' @param format string, can be 'tsv' or 'xml', and is only applicable for daily and unit value requests.  'tsv' returns results faster, but there is a possiblitiy that an incomplete file is returned without warning. XML is slower, 
#' but will offer a warning if the file was incomplete (for example, if there was a momentary problem with the internet connection). It is possible to safely use the 'tsv' option, 
#' but the user must carefully check the results to see if the data returns matches what is expected. The default is 'tsv'.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @return data dataframe with agency, site, dateTime, value, and code columns
#' @export
#' @keywords data import USGS web service
#' @examples
#' # These examples require an internet connection to run
#' siteNumber <- '04085427'
#' startDate <- '2012-01-01'
#' endDate <- '2012-06-30'
#' pCode <- '00060'
#' rawDailyQ <- retrieveNWISData(siteNumber,pCode, startDate, endDate)
#' rawDailyTemperature <- retrieveNWISData(siteNumber,'00010', startDate, endDate, StatCd='00001')
#' rawDailyTemperatureTSV <- retrieveNWISData(siteNumber,'00010', startDate, endDate, StatCd='00001',format='tsv')
#' rawDailyQAndTempMeanMax <- retrieveNWISData(siteNumber,c('00010','00060'), startDate, endDate, StatCd=c('00001','00003'))
retrieveNWISData <- function (siteNumber,ParameterCd,StartDate,EndDate,StatCd="00003",format="tsv",interactive=TRUE){  
  
  url <- constructNWISURL(siteNumber,ParameterCd,StartDate,EndDate,"dv",statCd=StatCd,format=format,interactive=interactive)
  
  if (format == "xml") {
    data <- getWaterML1Data(url)
    data$dateTime <- as.Date(data$dateTime)
  } else {
    data <- getRDB1Data(url,asDateTime=FALSE)
  }
  
  return (data)
}
