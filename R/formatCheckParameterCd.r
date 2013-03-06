#' formatCheckParameterCd
#'
#' Checks that the parameter code is 5 digits. If it is less, it will pad the string with zeros. If more, ask the user to re-enter.
#'
#' @param ParameterCd string to check
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @return ParameterCd string
#' @export
#' @examples
#' pCode <- '01234'
#' formatCheckParameterCd(pCode, interactive = FALSE)
formatCheckParameterCd <- function(ParameterCd, interactive=TRUE){     #checks for a 5 digit number
  if (nchar(ParameterCd) < 5){
    
    padVariable <- function(x,padTo){
      numDigits <- nchar(x)
      if (padTo != numDigits){
        leadingZeros <- paste(rep("0",(padTo-numDigits)),collapse="",sep="")
        x <- paste(leadingZeros,x,sep="")
      }
      return(x)
    }
    
    if (interactive){
      cat("Most USGS parameter codes are 5 digits long, you entered a ", nchar(ParameterCd), "digit number = ", ParameterCd , ".\n")
      
      ParameterCd <- padVariable(ParameterCd,5)
      cat("The following parameter code will be used instead:",ParameterCd,"\n")
      cat("If you would like to change the parameter code, enter it here (no quotes), otherwise hit return:\n")
      tempParameterCd <- readline()
      if (nzchar(tempParameterCd)) ParameterCd <- tempParameterCd
    } else {
      tempText <- padVariable(ParameterCd,5)
      warningMessage <- paste("Most USGS parameter codes are 5 digits long, you entered ", 
                              ParameterCd , ".\n",tempText," will be used instead", sep="")
      warning(warningMessage)
      ParameterCd <- padVariable(ParameterCd,5)
    }
  }
  return(ParameterCd)
}