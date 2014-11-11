#' Site Data Import from NWIS
#'
#' Returns a list of sites from the NWIS web service. This function gets the data from: \url{http://waterservices.usgs.gov/rest/Site-Test-Tool.html}.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service}
#' Mapper format is used
#'
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @keywords data import NWIS web service
#' @return retval dataframe with agency_cd, site_no, station_nm, site_tp_cd, dec_lat_va, and dec_long_va.
#' @export
#' @import XML
#' @examples
#' siteListPhos <- whatNWISsites(stateCd="OH",parameterCd="00665")
whatNWISsites <- function(...){
  
  matchReturn <- list(...)

  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse="",sep=""))))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- "http://waterservices.usgs.gov/nwis/site/?format=mapper&"
  urlCall <- paste(baseURL,
                   urlCall,sep = "")
  
  if(url.exists(urlCall)){
    h <- basicHeaderGatherer()
    doc = tryCatch({
      returnedDoc <- getURI(urlCall, headerfunction = h$update)
      if(h$value()["Content-Type"] == "text/xml;charset=UTF-8"){
        xmlTreeParse(returnedDoc, getDTD = FALSE, useInternalNodes = TRUE)
      } else {
        message(paste("URL caused an error:", urlCall))
        message("Content-Type=",h$value()["Content-Type"])
        return(NA)
      }   
      
    }, warning = function(w) {
      message(paste("URL caused a warning:", urlCall))
      message(w)
    }, error = function(e) {
      message(paste("URL does not seem to exist:", urlCall))
      message(e)
      return(NA)
    }) 
    
    doc <- xmlRoot(doc)
    numChunks <- xmlSize(doc)
    for(i in 1:numChunks){
      chunk <- doc[[1]]
      site_no <- as.character(xpathApply(chunk, "site/@sno"))
      station_nm <- as.character(xpathApply(chunk, "site/@sna"))
      site_tp_cd <- as.character(xpathApply(chunk, "site/@cat"))
      dec_lat_va <- as.numeric(xpathApply(chunk, "site/@lat"))
      dec_long_va <- as.numeric(xpathApply(chunk, "site/@lng"))
      agency_cd <- as.character(xpathApply(chunk, "site/@agc"))
      
      df <- data.frame(agency_cd, site_no, station_nm, site_tp_cd, 
                       dec_lat_va, dec_long_va, stringsAsFactors=FALSE) 
      
      if(1==i){
        retval <- df
      } else {
        retval <- rbind(retval, df)
      }
    }
    
    retval <- retval[!duplicated(retval),]
    
    retval$queryTime <- Sys.time()
    attr(retVal, "url") <- urlCall
    
    return(retval)
  } else {
    message("URL caused an error:", urlCall)
  }
}
