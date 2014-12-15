#' Site Data Import from NWIS
#'
#' Returns a list of sites from the NWIS web service. This function gets the data from: \url{http://waterservices.usgs.gov/rest/Site-Test-Tool.html}.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service}
#' Mapper format is used
#'
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @keywords data import NWIS web service
#' @return A data frame with at least the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' station_nm \tab character \tab Station name \cr
#' site_tp_cd \tab character \tab Site type code \cr
#' dec_lat_va \tab numeric \tab Decimal latitude \cr
#' dec_long_va \tab numeric \tab Decimal longitude \cr
#' queryTime \tab POSIXct \tab Query time \cr
#' }
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
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
  
  h <- basicHeaderGatherer()
  
  possibleError <- tryCatch({
    h <- basicHeaderGatherer()
    returnedDoc <- getURI(urlCall, headerfunction = h$update)      
  }, warning = function(w) {
    warning(w, "with url:", urlCall)
  }, error = function(e) {
    stop(e, "with url:", urlCall)
  }) 
  
  headerInfo <- h$value()

  if (headerInfo['status'] == "200"){
  
    doc <- xmlTreeParse(returnedDoc, getDTD = FALSE, useInternalNodes = TRUE)
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
    attr(retval, "url") <- urlCall
    attr(retval, "queryTime") <- Sys.time()
    
    return(retval)
  } else {
    stop("Status:", headerInfo['status'], ": ", headerInfo['statusMessage'], "\nFor: ", urlCall)
  }
  
}
