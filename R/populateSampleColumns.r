#' Populate Sample Columns
#'
#' Creates ConcAve and ConcLow based on Uncen. Removes any samples with NA values in ConcHigh
#'
#' @param rawData dataframe with dateTime, ConcLow, ConcHigh, Uncen
#' @return Sample2 dataframe
#' @export
#' @examples
#' dateTime <- c('1985-01-01', '1985-01-02', '1985-01-03')
#' ConcLow <- c(1,2,0)
#' ConcHigh <- c(1,2,3)
#' Uncen <- c(1,1,0)
#' dataInput <- data.frame(dateTime, ConcLow, ConcHigh, Uncen, stringsAsFactors=FALSE)
#' Sample <- populateSampleColumns(dataInput)
populateSampleColumns <- function(rawData){  # rawData is a dataframe with dateTime, ConcLow, ConcHigh, Uncen
  Sample <- as.data.frame(matrix(ncol=3,nrow=length(rawData$dateTime)))
  colnames(Sample) <- c('Date', 'ConcLow','ConcHigh')
  Sample$Date <- rawData$dateTime
  Sample$ConcLow <- rawData$ConcLow
  Sample$ConcHigh <- rawData$ConcHigh
  Sample$Uncen <- rawData$Uncen
  Sample$ConcAve <- (Sample$ConcLow+Sample$ConcHigh)/2
  Sample$ConcLow <- ifelse((rawData$ConcLow == 0.0 & rawData$Uncen == 0),NA,rawData$ConcLow)
  
  dateFrame <- populateDateColumns(rawData$dateTime)
  Sample <- cbind(Sample, dateFrame[,-1])
  
  Sample$SinDY <- sin(2*pi*Sample$DecYear)
  Sample$CosDY <- cos(2*pi*Sample$DecYear)
  Sample2 <- subset(Sample, (!is.na(Sample$ConcHigh)))  # Was just ConcHigh.....
  return (Sample2)  
}
