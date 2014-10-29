#' Pad string with leading zeros
#'
#' Function to pad a string with leading zeros. Useful for parameter codes and USGS site IDs.
#'
#' @param x string 
#' @param padTo number Final desired length of the string
#' @keywords data import USGS web service
#' @return x string returned with leading zeros
#' @export
#' @examples
#' pCode <- '10'
#' correctPCode <- zeroPad(pCode,5)
#' pCodes <- c('100','1000','0','12345','1565465465465465')
#' correctPCodes <- zeroPad(pCodes,5)
zeroPad <- function(x,padTo){
  if(padTo <= 1) return(x)
  numDigits <- nchar(x)
  padding <- padTo-numDigits
  padingZeros <- sapply(padding[padding > 0], function(y) paste(rep("0",y),collapse="",sep=""))
  x[padding > 0] <- paste(padingZeros,x[padding > 0],sep="")
  return(x)
}
