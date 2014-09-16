#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service}
#'
#' @param service string
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @keywords data import NWIS web service
#' @return retval dataframe 
#' @export
#' @examples
#' dataTemp <- getNWISData(stateCd="OH",parameterCd="00010")
getNWISData <- function(service="dv", ...){
  
  matchReturn <- match.call()
  
  values <- sapply(matchReturn[-1], function(x) URLencode(as.character(paste(eval(x),collapse="",sep=""))))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- paste0("http://waterservices.usgs.gov/nwis/",service,"/?format=rdb&")
  urlCall <- paste0(baseURL,urlCall)
  if(service=="qw"){
    urlCall <- paste0(urlCall,"&siteOutput=expanded")
  }
  
  retval <- getRDB1Data(urlCall)
  
  return(retval)
}
