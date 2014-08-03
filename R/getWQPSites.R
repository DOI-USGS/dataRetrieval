#' Site Data Import from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets the data from: \url{http://www.waterqualitydata.us}.
#' Arguments to the function should be based on \url{www.waterqualitydata.us/webservices_documentation.jsp}
#'
#' @param \dots see \url{www.waterqualitydata.us/webservices_documentation.jsp} for a complete list of options
#' @keywords data import WQP web service
#' @return retval dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on requested parameter codes)
#' @export
#' @examples
#' setInternet2(use=NA)
#' options(timeout=120)
#' \dontrun{sitesList <- getWQPSites(within=10,lat=43.06932,long=-89.4444,characteristicName="pH")
#' siteListPH <- getWQPSites(characteristicName="pH")}
getWQPSites <- function(...){
  
  matchReturn <- match.call()
  
  options <- c("bBox","lat","long","within","countrycode","statecode","countycode","siteType","organization",
    "siteid","huc","sampleMedia","characteristicType","characteristicName","pCode","activityId",
    "startDateLo","startDateHi","mimeType","Zip","providers")
  
  if(!all(names(matchReturn[-1]) %in% options)) warning(matchReturn[!(names(matchReturn[-1]) %in% options)],"is not a valid query parameter to the Water Quality Portal")
  
  values <- sapply(matchReturn[-1], function(x) URLencode(as.character(paste(x,collapse="",sep=""))))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- "http://www.waterqualitydata.us/Station/search?"
  urlCall <- paste(baseURL,
               urlCall,
               "&mimeType=tsv",sep = "")
  
  retval <- suppressWarnings(read.delim(urlCall, header = TRUE, quote="\"", dec=".", sep='\t', colClasses=c('character'), fill = TRUE))
  return(retval)
}
