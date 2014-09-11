#' General Data Import from Water Quality Portal
#'
#' Imports data from Water Quality Portal web service. This function gets the data from here: \url{http://www.waterqualitydata.us}.
#' because it allows for other agencies rather than the USGS.  
#'
#' @param \dots see \url{www.waterqualitydata.us/webservices_documentation.jsp} for a complete list of options
#' @keywords data import WQP web service
#' @return retval dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on requested parameter codes)
#' @export
#' @examples
#' nameToUse <- "pH"
#' pHData <- getGeneralWQPData(siteid="USGS-04024315",characteristicName=nameToUse)
getGeneralWQPData <- function(...){
  
  matchReturn <- match.call()
  
  options <- c("bBox","lat","long","within","countrycode","statecode","countycode","siteType","organization",
               "siteid","huc","sampleMedia","characteristicType","characteristicName","pCode","activityId",
               "startDateLo","startDateHi","mimeType","Zip","providers")
  
  if(!all(names(matchReturn[-1]) %in% options)) warning(matchReturn[!(names(matchReturn[-1]) %in% options)],"is not a valid query parameter to the Water Quality Portal")
  
  values <- sapply(matchReturn[-1], function(x) URLencode(as.character(paste(eval(x),collapse="",sep=""))))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- "http://www.waterqualitydata.us/Result/search?"
  urlCall <- paste(baseURL,
                   urlCall,
                   "&mimeType=tsv",sep = "")
  
  doc = tryCatch({
    h <- basicHeaderGatherer()
    doc <- getURL(urlCall, headerfunction = h$update)
    
  }, warning = function(w) {
    message(paste("URL caused a warning:", urlCall))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", urlCall))
    message(e)
    return(NA)
  })
  
  if(h$value()["Content-Type"] == "text/tab-separated-values;charset=UTF-8"){
    
    numToBeReturned <- as.numeric(h$value()["Total-Result-Count"])
    
    if (!is.na(numToBeReturned) | numToBeReturned != 0){
      retval <- read.delim(textConnection(doc), header = TRUE, quote="\"", 
                           dec=".", sep='\t', 
                           colClasses=c('character'), 
                           fill = TRUE)
      
      actualNumReturned <- nrow(retval)
      
      if(actualNumReturned != numToBeReturned) warning(numToBeReturned, " sample results were expected, ", actualNumReturned, " were returned")
      
      return(retval)
    } else {
      warning(paste("No data to retrieve from",urlCall))
      return(NA)
    }
    
  } else {
    message(paste("URL caused an error:", urlCall))
    message("Content-Type=",h$value()["Content-Type"])
    return(NA)
  }

  
}