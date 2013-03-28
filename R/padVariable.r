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
#' correctPCode <- padVariable(pCode,5)
padVariable <- function(x,padTo){
  numDigits <- nchar(x)
  if (padTo != numDigits){
    leadingZeros <- paste(rep("0",(padTo-numDigits)),collapse="",sep="")
    x <- paste(leadingZeros,x,sep="")
  }
  return(x)
}