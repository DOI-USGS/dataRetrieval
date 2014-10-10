#' USGS Site File Data Retrieval
#'
#' Imports data from USGS site file site. This function gets data from here: \url{http://waterservices.usgs.gov/}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#' # These examples require an internet connection to run
#' siteINFO <- getNWISSiteInfo('05114000')
#' siteINFOMulti <- getNWISSiteInfo(c('05114000','09423350'))
getNWISSiteInfo <- function(siteNumber){
  
  siteNumber <- paste(siteNumber,collapse=",")
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&siteOutput=Expanded&sites=",siteNumber,sep = "")
  
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
    
    INFO <- SiteFile[-1,]
    names(INFO) <- gsub("_",".",names(INFO))
    
    INFO$queryTime <- Sys.time()
    INFO$dec.lat.va <- as.numeric(INFO$dec.lat.va)
    INFO$dec.long.va <- as.numeric(INFO$dec.long.va)
    INFO$alt.va <- as.numeric(INFO$alt.va)
    
    return(INFO)
  } else {
    message(paste("URL caused an error:", urlSitefile))
    message("Content-Type=",h$value()["Content-Type"])
    return(NA)
  }
}
