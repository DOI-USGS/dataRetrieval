#' USGS Site File Data Retrieval
#'
#' Imports data from USGS site file site. This function gets data from here: \url{http://waterservices.usgs.gov/}
#'
#' @param siteNumbers string USGS site number.  This is usually an 8 digit number
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#' # These examples require an internet connection to run
#' siteINFO <- readNWISsite('05114000')
#' siteINFOMulti <- readNWISsite(c('05114000','09423350'))
readNWISsite <- function(siteNumbers){
  
  siteNumber <- paste(siteNumbers,collapse=",")
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&siteOutput=Expanded&sites=",siteNumber,sep = "")
  
  data <- importRDB1(urlSitefile,asDateTime=FALSE)
 
}
