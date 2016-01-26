#' Function to return data from web services
#'
#' This function accepts a url parameter, and returns the raw data. The function enhances
#' \code{\link[RCurl]{getURI}} with more informative error messages.
#'
#' @param obs_url character containing the url for the retrieval
#' @param \dots information to pass to header request
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr user_agent
#' @importFrom httr stop_for_status
#' @importFrom httr status_code
#' @importFrom httr headers
#' @export
#' @return raw data from web services
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
#' \dontrun{
#' rawData <- getWebServiceData(obs_url)
#' }
getWebServiceData <- function(obs_url, ...){
  
  r <- GET(obs_url, ..., user_agent(paste("dataRetrieval",packageVersion("dataRetrieval"),sep="/")))
  
  if(status_code(r) != 200){  
    message("For: ", obs_url,"\n")
    stop_for_status(r)
  } else {
    returnedDoc <- content(r, type="text")
    
    if(grepl("No sites/data found using the selection criteria specified", returnedDoc)){
      message(returnedDoc)
    }
    attr(returnedDoc, "headerInfo") <- headers(r)
    
    return(returnedDoc)
  }
  
#   possibleError <- tryCatch({
#     h <- basicHeaderGatherer()
#     
#     returnedDoc <- getURI(obs_url, headerfunction = h$update, 
#                           useragent = paste("dataRetrieval",packageVersion("dataRetrieval"),sep="/"), ...)      
#   }, warning = function(w) {
#     warning(w, "with url:", obs_url)
#   }, error = function(e) {
#     stop(e, "with url:", obs_url)
#   }) 
#   
#   headerInfo <- h$value()
#   
#   if(headerInfo['status'] != "200"){  
#     stop("Status:", headerInfo['status'], ": ", headerInfo['statusMessage'], "\nFor: ", obs_url)
#   } else {
#     if(grepl("No sites/data found using the selection criteria specified", returnedDoc)){
#       message(returnedDoc)
#       headerInfo['warn'] <- returnedDoc
#     }
#     attr(returnedDoc, "headerInfo") <- headerInfo
#     return(returnedDoc)
#   }
}