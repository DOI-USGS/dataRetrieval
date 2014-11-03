#' USGS data availability
#'
#' Imports a table of available parameters, period of record, and count.
#'
#' @param siteNumber string USGS site number.
#' @param service vector string. Options are "uv", "dv", "qw"
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @import RCurl
#' @import lubridate
#' @examples
#' availableData <- whatNWISdata('05114000')
#' # To find just unit value ('instantaneous') data:
#' uvData <- whatNWISdata('05114000',service="uv")
#' uvDataMulti <- whatNWISdata(c('05114000','09423350'),service=c("uv","dv"))
whatNWISdata <- function(siteNumber,service=c("uv","dv","qw")){
  
  siteNumber <- paste(siteNumber,collapse=",")
  
  service <- match.arg(service, c("dv","uv","qw","ad","id","pk","sv"), several.ok = TRUE)
  
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&seriesCatalogOutput=true&sites=",siteNumber,sep = "")
 
  doc = tryCatch({
    h <- basicHeaderGatherer()
    doc <- getURL(urlSitefile, headerfunction = h$update)
    
  }, warning = function(w) {
    message(paste("URL caused a warning:", urlSitefile))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", urlSitefile))
    message(e)
    return(NA)
  }) 
  
  if(h$value()["Content-Type"] == "text/plain;charset=UTF-8"){
    SiteFile <- read.delim(
      textConnection(doc),
      header = TRUE,
      quote="\"",
      dec=".",
      sep='\t',
      colClasses=c('character'),
      fill = TRUE,
      comment.char="#")
    
    SiteFile <- SiteFile[-1,]

    numberColumns <- grep("_va",names(SiteFile))    
    SiteFile[,numberColumns] <- sapply(SiteFile[,numberColumns],as.numeric)
    
    intColumns <- grep("_nu",names(SiteFile))
    SiteFile[,intColumns] <- sapply(SiteFile[,intColumns],as.integer)
    
    pCodes <- unique(SiteFile$parm_cd)
    
    parameterCdFile <- parameterCdFile
    
    pcodeINFO <- parameterCdFile[parameterCdFile$parameter_cd %in% pCodes,]
    SiteFile <- merge(SiteFile,pcodeINFO,by.x="parm_cd" ,by.y="parameter_cd",all=TRUE)
    SiteFile <- SiteFile[SiteFile$data_type_cd %in% service,]
    
    SiteFile$begin_date <- as.Date(parse_date_time(SiteFile$begin_date, c("Ymd", "mdY", "Y!")))
    SiteFile$end_date <- as.Date(parse_date_time(SiteFile$end_date, c("Ymd", "mdY", "Y!")))
    
    return(SiteFile)
  } else {
    message(paste("URL caused an error:", urlSitefile))
    message("Content-Type=",h$value()["Content-Type"])
    return(NA)
  }
}
