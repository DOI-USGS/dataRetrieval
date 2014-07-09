#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count.
#'
#' @param siteNumber string USGS site number.
#' @param type vector string. Options are "uv", "dv", "qw"
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#' # These examples require an internet connection to run
#' availableData <- getDataAvailability('05114000')
#' # To find just unit value ('instantaneous') data:
#' uvData <- availableData <- getDataAvailability('05114000',type="uv")
getDataAvailability <- function(siteNumber,type=c("uv","dv","qw")){
  
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
  
  pcodeINFO <- parameterCdFile[parameterCdFile$parameter_cd %in% pCodes,]
  SiteFile <- merge(SiteFile,pcodeINFO,by="parameter_cd")
  SiteFile <- SiteFile[SiteFile$service %in% type,]
  return(SiteFile)
}