#' Merge Sample and Daily Data for WRTDS
#'
#' Merges the flow data from the daily record into the sample record.
#'
#' @param localDaily dataframe containing the daily data, default is Daily
#' @param localSample dataframe containing the sample data, default is Sample
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS WRTDS
#' @export
#' @return newSample dataframe with merged flow information
#' @seealso \code{\link{getDVData}}, \code{\link{populateSampleColumns}}
#' @examples
#' # These examples require an internet connection to run
#' Daily <- getDVData('01594440','00060', '1985-01-01', '1985-03-31')
#' Sample <- getSampleData('01594440','01075', '1985-01-01', '1985-03-31')
#' Sample <- mergeReport()
mergeReport<-function(localDaily = Daily, localSample = Sample, interactive=TRUE){
  if (interactive){
    dataOverview(localDaily, localSample)  
  }
  
  newSample <- merge(Daily[,c("Date","Q","LogQ")],Sample,by = "Date",all.y = TRUE)

  return(newSample)
}
