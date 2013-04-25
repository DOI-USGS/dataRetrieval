#' Populate Concentration Columns
#'
#' Creates ConcLow, ConcHigh, Uncen (0 if censored, 1 if uncensored) columns for Sample data frame for WRTDS study.
#'
#' @param rawData vector with value and code columns
#' @return concentrationColumns dataframe
#' @export
#' @examples
#' code <- c("","<","")
#' value <- c(1,2,3)
#' dataInput <- data.frame(value, code, stringsAsFactors=FALSE)
#' concentrationDF <- populateConcentrations(dataInput)
populateConcentrations <- function(rawData){  # rawData is a dataframe with value, code
  concentrationColumns <- as.data.frame(matrix(ncol=3,nrow=length(rawData$value)))
  colnames(concentrationColumns) <- c('ConcLow','ConcHigh','Uncen')  
  concentrationColumns$ConcLow <- as.numeric(ifelse((rawData$code!="<" | is.na(rawData$code)),rawData$value,0))
  concentrationColumns$ConcHigh <- as.numeric(rawData$value)
  tempConcLow<-ifelse((rawData$code!="<" | is.na(rawData$code)),rawData$value,0)
  concentrationColumns$Uncen <- ifelse(tempConcLow==0,0,1)  
  #Add if value = NA?
  return (concentrationColumns)  # returns ConcLow, ConcHigh, Uncen (0 if censored, 1 if uncensored)
}
