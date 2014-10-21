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
  retval <- read.delim(  
    totalPath, 
    header = hasHeader,
    sep=separator,
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  if(ncol(retval) == 2){
    numCol <- 2
  } else {
    numCol <- seq(from = 3,to = ncol(retval), by = 2)
  }
  
  if(dateFormatCheck(retval[,1])){
    retval[,1] <- as.Date(retval[,1])  
  } else {
    retval[,1] <- as.Date(retval[,1],format="%m/%d/%Y")
  }
  
  retval[,numCol] <- sapply(numCol, function(x) as.numeric(retval[,x]))

  return (retval)
}
