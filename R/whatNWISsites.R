#' Site Data Import from NWIS
#'
#' Returns a list of sites from the NWIS web service. This function gets the data from: \url{http://waterservices.usgs.gov/rest/Site-Test-Tool.html}.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service}
#' Mapper format is used
#'
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @import utils
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
#' @importFrom XML xmlTreeParse
#' @importFrom XML xpathApply
#' @importFrom XML xmlSize
#' @importFrom xml2 xml_root
#' @importFrom xml2 xml_find_all
#' 
#' @examples
#' \dontrun{
#' siteListPhos <- whatNWISsites(stateCd="OH",parameterCd="00665")
#' }
whatNWISsites <- function(...){
  
  matchReturn <- list(...)
  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse=",",sep=""))))
  
  names(values)[names(values) == "siteNumber"] <- "sites"
  names(values)[names(values) == "siteNumbers"] <- "sites"
  
  urlCall <- drURL('site',Access=pkg.env$access, format="mapper", arg.list = values)


  rawData <- getWebServiceData(urlCall, encoding='gzip')

  doc <- xml_root(rawData)
  siteCategories <- xml_children(doc)
  for(sc in siteCategories){
    sites <- xml_children(sc)
    #attrs <- c("sno","sna","cat","lat","lng","agc")
    site_no <- xml_attr(sites, "sno")
    site_name <- xml_attr(sites, "sna")
    site_cat <- xml_attr(sites, "cat")
    dec_lat_va <- xml_attr(sites, "lat")
    dec_lon_va <- xml_attr(sites, "lng")
    agencyCd <- xml_attr(sites, "agc")
    
    if(xml_name(sc)=="colocated_sites"){
      colocated <- TRUE
    }else{
      colocated <- FALSE
    }
    
    
  }
  
  
  
  
  
  
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
  
}
