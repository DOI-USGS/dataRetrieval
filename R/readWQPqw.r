#' Raw Data Import for Water Quality Portal
#'
#' Imports data from the Water Quality Portal. 
#' This function gets the data from here: \url{http://www.waterqualitydata.us}. There
#' are four required input arguments: siteNumber, parameterCd, startDate, and endDate.
#' parameterCd can either be a USGS 5-digit code, or a characteristic name. The sites can be 
#' either USGS, or other Water Quality Portal offered sites. It is required to use the 'full'
#' site name, such as 'USGS-01234567'. 
#'
#' @param siteNumber string site number. This needs to include the full agency code prefix.
#' @param parameterCd vector of USGS 5-digit parameter code or string of characteristicNames. 
#' Leaving this blank will return all of the measured values during the specified time period.
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @keywords data import USGS web service
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally, a POSIXct dateTime column is supplied for 
#' start and end times.
#' @export
#' @import RCurl
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}}, 
#' \code{\link{readNWISqw}}, and \code{\link{importWQP}}
#' @examples
#' \dontrun{
#' rawPcode <- readWQPqw('USGS-01594440','01075', '', '')
#' rawCharacteristicName <- readWQPqw('WIDNR_WQX-10032762','Specific conductance', '', '')
#' }
readWQPqw <- function(siteNumber,parameterCd,startDate="",endDate=""){

  url <- constructWQPURL(siteNumber,parameterCd,startDate,endDate)
  retVal <- importWQP(url,TRUE)
  return(retVal)
  
}
