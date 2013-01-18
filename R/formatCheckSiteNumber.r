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