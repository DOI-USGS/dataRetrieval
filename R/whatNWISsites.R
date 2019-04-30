#' Site Data Import from NWIS
#'
#' Returns a list of sites from the NWIS web service. This function gets the data from: \url{https://waterservices.usgs.gov/rest/Site-Test-Tool.html}.
#' Mapper format is used
#'
#' @param \dots see \url{https://waterservices.usgs.gov/rest/Site-Service.html} for a complete list of options. A 
#' list (or lists) can also be supplied.
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
#' @importFrom xml2 xml_root
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_attr
#' 
#' @examples
#' \donttest{
#' siteListPhos <- whatNWISsites(stateCd="OH",parameterCd="00665")
#' oneSite <- whatNWISsites(sites="05114000")
#' }
whatNWISsites <- function(...){
  
  valuesList <- readNWISdots(...)

  values <- sapply(valuesList$values, function(x) URLencode(x))
  values["format"] <- "mapper" 

  urlCall <- drURL('site',Access=pkg.env$access, arg.list = values)

  rawData <- getWebServiceData(urlCall, encoding='gzip')

  doc <- xml_root(rawData)
  siteCategories <- xml_children(doc)
  retVal <- NULL
  for(sc in siteCategories){
    sites <- xml_children(sc)
    #attrs <- c("sno","sna","cat","lat","lng","agc")
    site_no <- xml_attr(sites, "sno")
    station_nm <- xml_attr(sites, "sna")
    site_tp_cd <- xml_attr(sites, "cat")
    dec_lat_va <- as.numeric(xml_attr(sites, "lat"))
    dec_long_va <- as.numeric(xml_attr(sites, "lng"))
    agency_cd <- xml_attr(sites, "agc")
    
    if(xml_name(sc)=="colocated_sites"){
      colocated <- TRUE
    }else{
      colocated <- FALSE
    }
    
    df <- data.frame(agency_cd, site_no, station_nm, site_tp_cd, 
                     dec_lat_va, dec_long_va, colocated, stringsAsFactors=FALSE) 
    
    if(is.null(retVal)){
      retVal <- df
    } else {
      retVal <- r_bind_dr(retVal, df)
    }
  }
  
  retVal <- retVal[!duplicated(retVal),]

  attr(retVal, "url") <- urlCall
  
  timenow <- Sys.time()
  
  attr(retVal, "queryTime") <- timenow
  # Backwards compatible, might remove later:
  retVal$queryTime <- timenow
  
  return(retVal)
}
