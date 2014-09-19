#' Import Sample Data for WRTDS
#'
#' Imports data from the Water Quality Portal, so it could be STORET, NWIS, or . This function gets the data from: \url{http://www.waterqualitydata.us}
#' For raw data, use getWQPData.  This function will retrieve the raw data, and compress it (summing constituents). See
#' chapter 7 of the EGRET user guide for more details, then converts it to the Sample dataframe structure.
#'
#' @param siteNumber string site number.  If USGS, it should be in the form :'USGS-XXXXXXXXX...'
#' @param characteristicName string
#' @param startDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS WRTDS
#' @export
#' @return Sample dataframe
#' @seealso \code{\link{getWQPData}}, \code{\link{getWQPSites}}, 
#' \code{\link{retrieveWQPqwData}}, \code{\link{retrieveNWISqwData}}, and \code{\link{basicWQPData}}, 
#' \code{\link{compressData}}, \code{\link{populateSampleColumns}}
#' @examples
#' # These examples require an internet connection to run
#' Sample_01075 <- getSTORETSampleData('USGS-01594440','Chloride', '', '')
#' Sample_All <- getSTORETSampleData('WIDNR_WQX-10032762','Specific conductance', '', '')
getSTORETSampleData <- function(siteNumber,characteristicName,startDate,endDate,interactive=TRUE){
  
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
  
  compressedData <- compressData(data, interactive=interactive)
  Sample <- populateSampleColumns(compressedData)
  return(Sample)
}
