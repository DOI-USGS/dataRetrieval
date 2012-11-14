#' Parameter Code Check
#'
#' Checks that the user-supplied parameter code is 5 digits. If not, asks the user to re-enter.
#'
#' @param ParameterCd string USGS parameter code
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import from web service
#' @return ParameterCd string
#' @export
#' @examples
#' formatCheckParameterCd('00060')
formatCheckParameterCd <- function(ParameterCd, interactive=TRUE){     #checks for a 5 digit number
  if (nchar(ParameterCd) != 5){
    if (interactive){
      cat("Most USGS parameter codes are 5 digits long, you entered a ", nchar(ParameterCd), "digit number = ", ParameterCd , ".\n")
      cat("If you would like to change the parameter code, enter it here (no quotes), otherwise hit return:\n")
      tempParameterCd <- readline()
      if (nzchar(tempParameterCd)) ParameterCd <- tempParameterCd
    } else {
      warningMessage <- paste("Most USGS parameter codes are 5 digits long, you entered ", ParameterCd , ".", sep="")
      warning(warningMessage)
    }
  }
  return(ParameterCd)
}