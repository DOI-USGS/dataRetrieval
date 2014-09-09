#' Data Import from Water Quality Portal
#'
#' Imports data from Water Quality Portal web service. This function gets the data from: \url{http://www.waterqualitydata.us}. This function is more general than getQWData
#' because it allows for other agencies rather than the USGS.  Therefore, the 5-digit parameter code cannot be used.
#' Instead, this function uses characteristicName.  A complete list can be found here 
#'
#' @param siteNumber string site number.  If USGS, it should be in the form :'USGS-XXXXXXXXX...'
#' @param characteristicName string
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import WQP web service
#' @return retval dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on requested parameter codes)
#' @export
#' @import RCurl
#' @examples
#' # These examples require an internet connection to run
#' Chloride <- getWQPData('USGS-01594440','Chloride', '', '')
#' SC <- getWQPData('WIDNR_WQX-10032762','Specific conductance', '', '')
#' NWIS_Cl <- getWQPData('USGS-04024000','30234', '', '')
#' MultipleQW <- getWQPData('USGS-04024000',c('30234','90095'), '', '')
getWQPData <- function(siteNumber,characteristicName,startDate,endDate,interactive=TRUE){
  
  retval <- retrieveWQPqwData(siteNumber=siteNumber,
                         parameterCd=characteristicName,
                         startDate=startDate,
                         endDate=endDate,
                         interactive=interactive)
  #Check for pcode:
  if(all(nchar(characteristicName) == 5)){
    suppressWarnings(pCodeLogic <- all(!is.na(as.numeric(characteristicName))))
  } else {
    pCodeLogic <- FALSE
  }

  if(nrow(retval) > 0){
    data <- processQWData(retval,pCodeLogic)
  } else {
    data <- NULL
  }
  
  return(data)
  
}
