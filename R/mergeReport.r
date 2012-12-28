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
#' Daily <- getDVData('01594440','00060', '1985-01-01', '1985-03-31', interactive=FALSE)
#' Sample <- getSampleData('01594440','01075', '1985-01-01', '1985-03-31', interactive=FALSE)
#' Sample <- mergeReport(interactive=FALSE)
mergeReport<-function(localDaily = Daily, localSample = Sample, interactive=TRUE){
  if (interactive){
    dataOverview(localDaily, localSample)  
  }
  julFirst<-localDaily$Julian[1]
  sampleDate<-as.Date(localSample$Date)
  sampleJulian<-as.numeric(julian(sampleDate,origin=as.Date("1850-01-01")))
  sampleIndex <- sampleJulian-julFirst+1
  Q <- localDaily$Q[sampleIndex]  
  LogQ<-log(Q)
  newSample<-data.frame(localSample,Q,LogQ)
  return(newSample)
}