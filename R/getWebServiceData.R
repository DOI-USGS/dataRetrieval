#' Function to return data from web services
#'
#' This function accepts a url parameter, and returns the raw data. The function enhances
#' \code{\link[httr]{GET}} with more informative error messages.
#'
#' @param obs_url character containing the url for the retrieval
#' @param \dots information to pass to header request
#' @importFrom httr GET
#' @importFrom httr POST
#' @importFrom httr RETRY
#' @importFrom httr user_agent
#' @importFrom httr stop_for_status
#' @importFrom httr status_code
#' @importFrom httr headers
#' @importFrom httr content
#' @importFrom httr content_type
#' @importFrom curl curl_version
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
  
  returnedList <- tryCatch({
    retryGetOrPost(obs_url, ...)
  }, error = function(e){
    NULL
  })
    
  if(is.null(returnedList)){
    message("Switching from https to http")
    obs_url <- gsub("https", "http", obs_url)
    returnedList <- tryCatch({
      retryGetOrPost(obs_url, ...)
    }, error = function(e){
      NULL
    })
  } 
  
  if(status_code(returnedList) != 200){
    message("For: ", obs_url,"\n")
    stop_for_status(returnedList)
  } else {
    
    headerInfo <- headers(returnedList)

    if(headerInfo$`content-type` == "text/tab-separated-values;charset=UTF-8"){
      returnedDoc <- content(returnedList, type="text",encoding = "UTF-8")
    } else if (headerInfo$`content-type` %in% 
               c("application/zip", "application/zip;charset=UTF-8")) {
      returnedDoc <- returnedList
    } else if (headerInfo$`content-type` %in% c("text/html","text/html; charset=UTF-8") ){
      txt <- readBin(returnedList$content, character())
      message(txt)
      return(txt)
      
    } else {
      returnedDoc <- content(returnedList,encoding = "UTF-8")
      if(grepl("No sites/data found using the selection criteria specified", returnedDoc)){
        message(returnedDoc)
      }
    }

    attr(returnedDoc, "headerInfo") <- headerInfo

    return(returnedDoc)
  }
}

default_ua <- function() {
  versions <- c(
    libcurl = curl_version()$version,
    httr = as.character(packageVersion("httr")),
    dataRetrieval = as.character(packageVersion("dataRetrieval"))
  )
  paste0(names(versions), "/", versions, collapse = " ")
}

#' getting header information from a WQP query
#'
#'@param url the query url
#'@importFrom httr HEAD
#'@importFrom httr headers
getQuerySummary <- function(url){
  queryHEAD <- HEAD(url)
  retquery <- headers(queryHEAD)
  countNames <- c('total-site-count', 'nwis-site-count', 'total-result-count', 'nwis-result-count')
  retquery[which(names(retquery) %in% countNames)] <- unlist(lapply(countNames, retquery = retquery,
                                                                    FUN = function(c, retquery){
                                                                      retquery[[c]] <- as.numeric(retquery[[c]])
                                                                      return(retquery[c])
                                                                    }))
  retquery$date <- as.Date(retquery$date, format = "%a, %d %b %Y %H:%M:%S")
  return(retquery)
}

retryGetOrPost <- function(url, ...) {
  resp <- NULL
  if (nchar(url) < 2048) {
    resp <- RETRY("GET", url, ..., user_agent(default_ua()))
  } else {
    split <- strsplit(url, "?", fixed=TRUE)
    url <- split[[1]][1]
    query <- split[[1]][2]
    resp <- RETRY("POST", url, ..., body = query,
          content_type("application/x-www-form-urlencoded"), user_agent(default_ua()))
  }
  return(resp)
}