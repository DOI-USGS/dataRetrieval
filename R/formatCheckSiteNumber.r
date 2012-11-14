#' Site number check
#'
#' Checks that the user-supplied site number is 8 digits, typical for many USGS station site id's.If not, asks the user to re-enter.
#' The final siteNumber can be more or less than 8 digits, since there are conditions where that is the case.
#'
#' @param siteNumber string USGS site number
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords error checking for data import from web service
#' @return siteNumber string
#' @export
#' @examples
#' formatCheckSiteNumber('01594440')
#' formatCheckSiteNumber('015944400',interactive=FALSE)
formatCheckSiteNumber <- function(siteNumber, interactive=TRUE){  #checks for a 8 digit number
  if (nchar(siteNumber) != 8){
    if (interactive){
      cat("Most common USGS site numbers are 8 digits long, you entered a ", nchar(siteNumber), "digit number = ", siteNumber , ".\n")
      cat("If you would like to change the site id, enter it here (no quotes), otherwise hit return:\n")
      tempSiteID <- readline()
      if (nzchar(tempSiteID)) siteNumber <- tempSiteID
    } else {
      warningMessage <- paste("Most common USGS site numbers are 8 digits long, you entered ", siteNumber , ".", sep="")
      warning(warningMessage)
    }
  }
  return(siteNumber)
}