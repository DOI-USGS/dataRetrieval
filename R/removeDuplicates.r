#' Remove Duplicates
#'
#' Removes observations from the data frame Sample when the observation has the identical date and value as another observation
#'
#' @param Sample dataframe with at least DecYear and ConcHigh, default name is Sample
#' @export
#' @return Sample1 dataframe
#' @examples
#' DecYear <- c('1985.01', '1985.01', '1985.02', '1985.02', '1985.03')
#' ConcHigh <- c(1,2,3,3,5)
#' dataInput <- data.frame(DecYear, ConcHigh, stringsAsFactors=FALSE)
#' removeDuplicates(dataInput)
removeDuplicates <- function(Sample) {  
  Sample1 <- Sample[!duplicated(Sample[c("DecYear","ConcHigh")]),]
  
  return(Sample1)
}
