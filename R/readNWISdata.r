#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov} service calls.
#'
#' @param service string. Possible values are "iv" (for instantaneous), "dv" (for daily values), "gwlevels" 
#' (for groundwater levels), and "qwdata" (for water quality)
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @keywords data import NWIS web service
#' @return retval dataframe 
#' @export
#' @examples
#' dataTemp <- readNWISdata(stateCd="OH",parameterCd="00010")
#' dataTempUnit <- readNWISdata(sites="03086500", service="iv", parameterCd="00010")
#' #Empty:
#' multiSite <- readNWISdata(sites=c("04025000","04072150"), service="iv", parameterCd="00010")
#' #Not empty:
#' multiSite <- readNWISdata(sites=c("04025500","040263491"), service="iv", parameterCd="00060")
readNWISdata <- function(service="dv", ...){
  
  matchReturn <- list(...)
  
  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse=",",sep=""))))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  if(service %in% c("dv","iv","gwlevels")){
    format <- "waterml"
  } else {
    format <- "rdb1,1"
  }
  
  baseURL <- paste0("http://waterservices.usgs.gov/nwis/",service,"/?format=",format,"&")
  urlCall <- paste0(baseURL,urlCall)
  
  if(service=="qwdata"){
    urlCall <- paste0(urlCall,"&siteOutput=expanded")
    retval <- importRDB1(urlCall)
  } else {

    retval <- importWaterML1(urlCall, asDateTime = ("iv" == service))
  }
  
  
  return(retval)
}
