#' Convert Preloaded Flow Data
#'
#' Imports data from data already pre-loaded into the R workspace. Specifically used to import water flow data for use in the WRTDS package.
#' For WRTDS usage, the first column is expected to be dates, the second column measured values.
#' The third column is optional, it contains any remark codes.
#'
#' @param rawData dataframe data already loaded into R workspace
#' @keywords data import
#' @return retval dataframe with dateTime, value, and code columns
#' @export
#' @examples
#' rawData <- getPreLoadedData(ChoptankRiverFlow)
getPreLoadedData <- function (rawData){  
  retval <- as.data.frame(rawData, stringsAsFactors=FALSE)
  if(ncol(retval) == 2){
    names(retval) <- c('dateTime', 'value')
  } else if (ncol(retval) == 3){
    names(retval) <- c('dateTime', 'value', 'code')
  }
  retval$dateTime <- as.character(retval$dateTime)
  if(dateFormatCheck(retval$dateTime)){
    retval$dateTime <- as.Date(retval$dateTime)  
  } else {
    retval$dateTime <- as.Date(retval$dateTime,format="%m/%d/%Y")
  }
  
  retval$value <- as.numeric(retval$value)
  return (retval)
}