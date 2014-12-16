#' Function to return data from web services
#'
#' This function accepts a url parameter, and returns the raw data. The function enhances
#' \code{\link[RCurl]{getURI}} with more informative error messages.
#'
#' @param obs_url character containing the url for the retrieval
#' @import RCurl
#' @export
#' @return raw data from web services
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
#' rawData <- getWebServiceData(obs_url)
getWebServiceData <- function(obs_url){
  possibleError <- tryCatch({
    h <- basicHeaderGatherer()
    returnedDoc <- getURI(obs_url, headerfunction = h$update)      
  }, warning = function(w) {
    warning(w, "with url:", obs_url)
  }, error = function(e) {
    stop(e, "with url:", obs_url)
  }) 
  
  headerInfo <- h$value()
  
  if(headerInfo['status'] != "200"){  
    stop("Status:", headerInfo['status'], ": ", headerInfo['statusMessage'], "\nFor: ", obs_url)
  } else {
    attr(returnedDoc, "headerInfo") <- headerInfo
    return(returnedDoc)
  }
}