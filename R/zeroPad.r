#' Pad string with leading zeros
#'
#' Function to pad a string with leading zeros. Useful for parameter codes and USGS site IDs.
#'
#' @param x character 
#' @param padTo number Final desired length of the character
#' @keywords data import USGS web service
#' @return x character returned with leading zeros
#' @export
#' @examples
#' pCode <- '10'
#' correctPCode <- zeroPad(pCode,5)
#' pCodes <- c('100','1000','0','12345','1565465465465465')
#' correctPCodes <- zeroPad(pCodes,5)
#' pCodeNA <- c(1,2,NA)
#' padPCodeNA <- zeroPad(pCodeNA,4)
zeroPad <- function(x,padTo){
  if(padTo <= 1) return(x)
  #if(is.na(x) == TRUE) return(x)
  numDigits <- nchar(x)
  padding <- padTo-numDigits
  padNA <- which(is.na(padding) == TRUE)
  if(length(padNA > 1)){
    padding[padNA] <- 0
    message("There were NAs send to zeroPad")
  }
  paddingZeros <- vapply(
      X = padding[padding > 0], 
      FUN = function(y) paste(rep("0",y),collapse="",sep=""),
      FUN.VALUE = ""
  )
  x[padding > 0] <- paste(paddingZeros,x[padding > 0],sep="")
  return(x)
}
