#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count. There is also an option to load the long parameter names and additional information on the parameters with longNames=TRUE.
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param interactive logical Option for interactive mode.  If true, a progress indicator is printed to the console.
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#' # These examples require an internet connection to run
#' availableData <- getDataAvailability('05114000')
getDataAvailability <- function(siteNumber="",interactive=TRUE){
  
  # Checking for 8 digit site ID:
  siteNumber <- formatCheckSiteNumber(siteNumber,interactive=interactive)
  
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&seriesCatalogOutput=true&sites=",siteNumber,sep = "")
  
  SiteFile <- read.delim(  
    urlSitefile, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  SiteFile <- SiteFile[-1,]
  
  SiteFile <- with(SiteFile, data.frame(parameter_cd=parm_cd, statCd=stat_cd, startDate=begin_date,endDate=end_date, count=count_nu,service=data_type_cd,stringsAsFactors = FALSE))
  
  SiteFile <- SiteFile[!is.na(SiteFile$parameter_cd),]
  SiteFile <- SiteFile["" != SiteFile$parameter_cd,]
  SiteFile$startDate <- as.Date(SiteFile$startDate)
  SiteFile$endDate <- as.Date(SiteFile$endDate)
  SiteFile$count <- as.numeric(SiteFile$count)
  
  pCodes <- unique(SiteFile$parameter_cd)

  pcodeINFO <- getParameterInfo(pCodes,interactive)
  SiteFile <- merge(SiteFile,pcodeINFO,by="parameter_cd")
  
  return(SiteFile)
}
