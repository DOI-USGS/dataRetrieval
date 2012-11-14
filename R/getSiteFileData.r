#' USGS Site File Data Retrieval
#'
#' Imports data from USGS site file site. This function gets data from here: \url{http://waterservices.usgs.gov/}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return retval dataframe with all information found in the expanded site file
#' @export
#' @examples
#' # These examples require an internet connection to run
#' getSiteFileData('05114000',interactive=FALSE)
getSiteFileData <- function(siteNumber="",interactive=TRUE){
  siteNumber <- formatCheckSiteNumber(siteNumber, interactive=interactive)
  urlSitefile <- paste("http://waterservices.usgs.gov/nwis/site?format=rdb&siteOutput=Expanded&sites=",siteNumber,sep = "")
  SiteFile <- read.delim(  
    urlSitefile, 
    header = FALSE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  col.nm <- make.names(unlist(SiteFile[1,, drop=TRUE]), allow_=FALSE)
  retval <- lapply(SiteFile, function(x) {
    Typ <- x[2] # The type
    x <- x[-c(1,2)] # the data
    if(regexpr('d$', Typ) > 0) { # Must be date
      ret.val <- try(as.Date(x)) # The data are in standard format, but...
      if(class(ret.val) == "try-error")
        ret.val <- x
    }
    else if(regexpr('n$', Typ) > 0) # Must be numeric
      ret.val <- as.numeric(x)
    else # Must be character
      ret.val <- x
    return(ret.val)})
  INFO <- as.data.frame(retval, stringsAsFactors=FALSE)
  names(INFO) <- col.nm
  INFO$queryTime <- Sys.time()
  INFO$dec.lat.va <- as.numeric(INFO$dec.lat.va)
  INFO$dec.long.va <- as.numeric(INFO$dec.long.va)
  INFO$alt.va <- as.numeric(INFO$alt.va)
  
  return(INFO)
}