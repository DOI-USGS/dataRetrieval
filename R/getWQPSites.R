#' Site DAta Import from Water Quality Portal
#'
#' Imports site data from Water Quality Portal web service. This function gets the data from here: \url{http://www.waterqualitydata.us}. This function is more general than getQWData
#' because it allows for other agencies rather than the USGS.  Therefore, the 5-digit parameter code cannot be used.
#' Instead, this function uses characteristicName.  A complete list can be found here \url{http://www.waterqualitydata.us/Codes/Characteristicname}
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
