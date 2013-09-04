#' Data Import for USGS NWIS Water Quality Data
#'
#' Imports and processes data from NWIS web service. This function gets the data from here: \url{http://www.waterqualitydata.us}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param ParameterCd string USGS parameter code.  This is usually an 5 digit number. Multiple parameter codes can be inputted with a ';' separator.  Leaving this blank will return all of the measured values during the specified time period.
#' @param StartDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param EndDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return retval dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on requested parameter codes)
#' @export
#' @examples
#' # These examples require an internet connection to run
#' rawProcessedSample <- getQWData('01594440','01075', '1985-01-01', '1985-03-31')
#' rawProcessedSampleAll <- getQWData('05114000','', '1985-01-01', '1985-03-31')
#' rawProcessedSampleSelect <- getQWData('05114000','00915;00931', '1985-01-01', '1985-04-30')
getQWData <- function(siteNumber,ParameterCd,StartDate,EndDate,interactive=TRUE){
  rawSample <- getRawQWData(siteNumber,ParameterCd,StartDate,EndDate,interactive)
  retval <- processQWData(rawSample)
  return(retval)
}
