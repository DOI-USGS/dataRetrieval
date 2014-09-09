#' formatCheckParameterCd
#'
#' Checks that the parameter code is 5 digits. If it is less, it will pad the string with zeros. If more, ask the user to re-enter.
#'
#' @param parameterCd string to check
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @return parameterCd string
#' @export
#' @examples
#' pCode <- '01234'
#' formatCheckParameterCd(pCode)
formatCheckParameterCd <- function(parameterCd, interactive=TRUE){     #checks for a 5 digit number
  
  pCodeReturn <- rep(NA,length(parameterCd))
  index <- 1
  
  for (i in parameterCd){
  
    if (nchar(i) < 5){      
      if (interactive){
        message("Most USGS parameter codes are 5 digits long, you entered a ", nchar(i), " digit number = ", i , ".\n")
        
        i <- padVariable(i,5)
        message("The following parameter code will be used instead:",i,"\n")
        message("If you would like to change the parameter code, enter it here (no quotes), otherwise hit return:\n")
        tempparameterCd <- readline()
        if (nzchar(tempparameterCd)){
          i <- tempparameterCd
        }
      } else {
        tempText <- padVariable(i,5)
        warningMessage <- paste("Most USGS parameter codes are 5 digits long, you entered ", 
                                i , ".\n",tempText," will be used instead", sep="")
        warning(warningMessage)
        i <- padVariable(i,5)
      }
      
    } 
    pCodeReturn[index] <- i
    index <- index + 1
  }
  return(pCodeReturn)
}
