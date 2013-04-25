#' Import Preloaded Daily Data for WRTDS use
#'
#' This function is intended to convert daily data that is already loaded into the R enviornment to the WRTDS daily data frame.
#'
#' @param loadedData dataframe that is already loaded in R session
#' @param qUnit number 1 is cubic feet per second, 2 is cubic meters per second, 3 is 10^3 cubic feet per second, and 4 is 10^3 cubic meters per second
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import  WRTDS
#' @export
#' @return Daily dataframe 
#' @examples
#' Daily <- getPreLoadedDailyData(ChoptankRiverFlow, interactive=FALSE)
getPreLoadedDailyData <- function (loadedData,qUnit=1,interactive=TRUE){
  data <- getPreLoadedData(loadedData)
  convertQ<-c(35.314667,1)
  qConvert<-convertQ[qUnit]
  if (interactive){
    if(qUnit==1) cat("\n the input discharge are assumed to be in cubic feet per second\nif they are in cubic meters per second, then the call to getPreLoadedDailyData should specify qUnit=2\n")
  }
  localDaily <- populateDaily(data,qConvert, interactive=interactive)
  return(localDaily)
}
