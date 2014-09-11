#' Basic Data Import for Water Flow Data
#'
#' Imports data from user-supplied data file. Specifically used to import water flow data for use in the WRTDS package.
#' For WRTDS usage, the first column is expected to be dates, the second column measured values.
#' The third column is optional, it contains any remark codes.
#'
#' @param filePath string specifying the path to the file
#' @param fileName string name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator string character that separates data cells
#' @keywords data import file
#' @return retval dataframe with dateTime, value, and code columns
#' @export
#' @examples
#' # Examples of how to use getDataFromFile:
#' # Change the file path and file name to something meaningful:
#' filePath <- system.file("extdata", package="dataRetrieval")
#' filePath <- paste(filePath,"/",sep="")
#' fileName <- 'ChoptankRiverFlow.txt'
#' ChopData <- getDataFromFile(filePath,fileName, separator="\t")
getDataFromFile <- function (filePath,fileName,hasHeader=TRUE,separator=","){
  totalPath <- paste(filePath,fileName,sep="");  
  tmp <- read.delim(  
    totalPath, 
    header = hasHeader,
    sep=separator,
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  retval <- as.data.frame(tmp, stringsAsFactors=FALSE)
  if(ncol(retval) == 2){
    names(retval) <- c('dateTime', 'value')
  } else if (ncol(retval) == 3){
    names(retval) <- c('dateTime', 'value', 'code')
  }
  
  if(dateFormatCheck(retval$dateTime)){
    retval$dateTime <- as.Date(retval$dateTime)  
  } else {
    retval$dateTime <- as.Date(retval$dateTime,format="%m/%d/%Y")
  }
  
  retval$value <- as.numeric(retval$value)
  return (retval)
}
