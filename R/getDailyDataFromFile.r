#' Import Daily Data for WRTDS
#'
#' Imports data from a user-supplied file, and converts it to a Daily data frame, appropriate for WRTDS calculations.
#'
#' @param filePath string specifying the path to the file
#' @param fileName string name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator string character that separates data cells
#' @param qUnit number 1 is cubic feet per second, 2 is cubic meters per second, 3 is 10^3 cubic feet per second, and 4 is 10^3 cubic meters per second
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import file
#' @keywords data import USGS WRTDS
#' @export
#' @return Daily dataframe
#' @examples
#' # Examples of how to use getDailyDataFromFile:
#' # Change the file path and file name to something meaningful:
#' #filePath <-  '~/RData/'  # Sample format
#' fileName <- 'ChoptankRiverFlow.txt'
#' #getDailyDataFromFile(filePath,fileName,separator="\t")
getDailyDataFromFile <- function (filePath,fileName,hasHeader=TRUE,separator=",",qUnit=1,interactive=TRUE){
  data <- getDataFromFile(filePath,fileName,hasHeader=hasHeader,separator=separator)
  convertQ<-c(35.314667,1,0.035314667,0.001)
  qConvert<-convertQ[qUnit]
  if (interactive){
    if(qUnit==1) cat("\n the input discharge are assumed to be in cubic feet per second\nif they are in cubic meters per second, then the call to getDailyDataFromFile should specify qUnit=2\n")
  }
  localDaily <- populateDaily(data,qConvert, interactive=interactive)
  return(localDaily)
}