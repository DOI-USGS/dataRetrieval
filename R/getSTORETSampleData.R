#' Import Sample Data for WRTDS
#'
#' Imports data from the Water Quality Portal. This function gets the data from: \url{http://www.waterqualitydata.us}
#' For raw data, use getWQPData.  This function will retrieve the raw data, and compress it (summing constituents). See
#' chapter 7 of the EGRET user guide for more details, then converts it to the Sample dataframe structure.
#'
#' @param siteNumber string site number.  If USGS, it should be in the form :'USGS-XXXXXXXXX...'
#' @param characteristicName string
#' @param StartDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param EndDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS WRTDS
#' @export
#' @return Sample dataframe
#' @seealso \code{\link{compressData}}, \code{\link{populateSampleColumns}}
#' @examples
#' # These examples require an internet connection to run
#' Sample_01075 <- getSTORETSampleData('USGS-01594440','Chloride', '', '')
#' Sample_All <- getSTORETSampleData('WIDNR_WQX-10032762','Specific conductance', '', '')
getSTORETSampleData <- function(siteNumber,characteristicName,StartDate,EndDate,interactive=TRUE){
  data <- getWQPData(siteNumber,characteristicName,StartDate,EndDate,interactive=interactive)
  compressedData <- compressData(data, interactive=interactive)
  Sample <- populateSampleColumns(compressedData)
  return(Sample)
}
