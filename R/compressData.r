#' Compress sample data frame
#'
#' Using raw data that has at least dateTime, value, code, populates the measured data portion of the Sample data frame used in WRTDS
#' ConcLow  = Lower bound for an observed concentration
#' ConcHigh = Upper bound for an observed concentration
#' ConcAve  = Average of ConcLow and ConcHigh.  If ConcLow is NA, then ConcAve = ConcHigh/2
#' Uncen    = 1 if uncensored, 0 if censored
#'
#' @param data dataframe contains at least dateTime, value, code columns
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @return dataframe returnDataFrame data frame containing dateTime, ConcHigh, ConcLow, Uncen, ConcAve
#' @export
#' @examples
#' dateTime <- c('1985-01-01', '1985-01-02', '1985-01-03')
#' comment1 <- c("","","")
#' value1 <- c(1,2,3)
#' comment2 <- c("","<","")
#' value2 <- c(2,3,4)
#' comment3 <- c("","","<")
#' value3 <- c(3,4,5)
#' dataInput <- data.frame(dateTime, comment1, value1, 
#'       comment2, value2, 
#'       comment3, value3, stringsAsFactors=FALSE)
#' compressData(dataInput)
compressData <- function(data, interactive=TRUE){  
  
  data <- as.data.frame(data, stringsAsFactors=FALSE)
  numColumns <- ncol(data)
  numDataColumns <- (numColumns-1)/2
  lowConcentration <- rep(0,nrow(data))
  highConcentration <- rep(0,nrow(data))
  uncensored <- rep(0,nrow(data))
  
  i <- 1
  while (i <= numDataColumns) {
    code <- data[2*i]
    value <- data[2*i+1]
    value <- as.numeric(unlist(value))
    value[is.na(value)] <- 0
    returnDataFrame <- as.data.frame(matrix(ncol=2,nrow=nrow(code)))
    colnames(returnDataFrame) <- c('code','value')
    returnDataFrame$code <- code[[1]]
    returnDataFrame$code <- ifelse(is.na(returnDataFrame$code),"",returnDataFrame$code)
    returnDataFrame$value <- value
    concentrationColumns <- populateConcentrations(returnDataFrame)
    lowConcentration <- lowConcentration + concentrationColumns$ConcLow
    highConcentration <- highConcentration + concentrationColumns$ConcHigh
    i <- i + 1
  }
  
  names(data) <- c('dateTime', 'code', 'value')
  returnDataFrame <- as.data.frame(matrix(ncol=3,nrow=nrow(data)))
  names(returnDataFrame) <- c('dateTime', 'ConcLow', 'ConcHigh')
  
  data$dateTime <- as.character(data$dateTime)
  if(dateFormatCheck(data$dateTime)){
    returnDataFrame$dateTime <- as.Date(data$dateTime)  
  } else {
    data$dateTime <- as.Date(data$dateTime,format="%m/%d/%Y")
    returnDataFrame$dateTime <- as.Date(data$dateTime,format="%m/%d/%Y")
  }
  returnDataFrame$ConcLow <- as.numeric(lowConcentration)
  returnDataFrame$ConcHigh <- as.numeric(highConcentration)
  Uncen1<-ifelse(returnDataFrame$ConcLow==returnDataFrame$ConcHigh,1,0)
  returnDataFrame$Uncen<-ifelse(is.na(returnDataFrame$ConcLow),0,Uncen1)
  
  flaggedData1 <- returnDataFrame[(returnDataFrame$ConcLow == 0 & returnDataFrame$ConcHigh == 0),]
  returnDataFrame <- returnDataFrame[!(returnDataFrame$ConcLow == 0 & returnDataFrame$ConcHigh == 0),]
  
  if (nrow(flaggedData1) > 0){
    WarningMessage <- paste("Deleted ", nrow(flaggedData1), " rows of data because concentration was reported as 0.0, the program is unable to interpret that result and is therefore deleting it.", sep="")    
    warning(WarningMessage)
    if (interactive){
      cat("Deleted Rows:\n")
      print(flaggedData1)
    }
  }
  
  flaggedData2 <- returnDataFrame[(returnDataFrame$ConcLow > returnDataFrame$ConcHigh),]
  returnDataFrame <- returnDataFrame[(returnDataFrame$ConcLow <= returnDataFrame$ConcHigh),]
  
  if (nrow(flaggedData2) > 0){
    WarningMessage <- paste("Deleted ", nrow(flaggedData2), " rows of data because the high concentration was reported lower than the low concentration, the program is unable to interpret that result and is therefore deleting it.", sep="")    
    warning(WarningMessage)
    if (interactive){
      cat("Deleted Rows:\n")
      print(flaggedData2)
    }
  }
  
  return(returnDataFrame)
}
