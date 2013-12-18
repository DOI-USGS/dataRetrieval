#' formatCheckSiteNumber
#'
#' Checks that the site code is at least 8 digits. If not, it confirms with the user.
#' @param siteNumber string to check
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords WRTDS flow
#' @return siteNumber string
#' @export
#' @examples
#' site<- '01234567'
#' formatCheckSiteNumber(site)
#' site_incorrect <- '1234567'
#' formatCheckSiteNumber(site_incorrect)
formatCheckSiteNumber <- function(siteNumber, interactive=TRUE){  #checks for a 8 digit number
  if (nchar(siteNumber) < 8){
    if (interactive){
      cat("Most common USGS site numbers are at least 8 digits long, you entered a ", nchar(siteNumber), "digit number = ", siteNumber , ".\n")
      cat("If you would like to change the site id, enter it here (no quotes), otherwise hit return:\n")
      tempSiteID <- readline()
      if (nzchar(tempSiteID)) siteNumber <- tempSiteID
    } else {
      warningMessage <- paste("Most common USGS site numbers are at least 8 digits long, you entered ", siteNumber , ".", sep="")
      warning(warningMessage)
    }
  }
  return(siteNumber)
}
