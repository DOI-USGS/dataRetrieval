#' Basic Data Import
#'
#' Imports data from user-supplied data file. Specifically used to import water quality data for use in the WRTDS package.
#' For WRTDS usage, the first column is expected to be dates, the second column remarks (specifically < if censored data),
#' and the third column is measured values.  There can be additional columns of data, for each column of data, there should
#' be a remark column preceeding.
#'
#' @param filePath string specifying the path to the file
#' @param fileName string name of file to open
#' @param hasHeader logical true if the first row of data is the column headers
#' @param separator string character that separates data cells
#' @keywords data import file
#' @return retval dataframe
#' @export
#' @examples
#' # Examples of how to use getQWDataFromFile:
#' # Change the file path and file name to something meaningful:
#' filePath <- system.file("extdata", package="dataRetrieval")
#' filePath <- paste(filePath,"/",sep="")
#' fileName <- 'ChoptankRiverNitrate.csv'
#' rawSampleData <- getQWDataFromFile(filePath,fileName, separator=";")
getQWDataFromFile <- function (filePath,fileName,hasHeader=TRUE,separator=","){
  totalPath <- paste(filePath,fileName,sep="")
  tmp <- read.delim(  
    totalPath, 
    header = hasHeader,
    sep=separator,
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  retval <- as.data.frame(tmp, stringsAsFactors=FALSE)
  names(retval)[1:3] <- c("dateTime","code","value")
  
  if(dateFormatCheck(retval$dateTime[1])){
    retval$dateTime <- as.Date(retval$dateTime)
    
    compressedData <- compressData(retval)
    Sample <- populateSampleColumns(compressedData)
    return (Sample)    
  } else {
    warning("Please adjust date formatting to 'YYYY-MM-DD'")
    return(NA)
  }

}
