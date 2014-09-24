#' Site Data Import from NWIS
#'
#' Returns a list of sites from the NWIS web service. This function gets the data from: \url{http://waterservices.usgs.gov/rest/Site-Test-Tool.html}.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service}
#'
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @keywords data import NWIS web service
#' @return retval dataframe 
#' @export
#' @examples
#' siteListPhos <- getNWISSites(stateCd="OH",parameterCd="00665")
getNWISSites <- function(...){
  
  matchReturn <- list(...)

  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse="",sep=""))))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- "http://waterservices.usgs.gov/nwis/site/?format=rdb&"
  urlCall <- paste(baseURL,
                   urlCall,sep = "")

  retval <- getRDB1Data(urlCall)

  return(retval)
}
