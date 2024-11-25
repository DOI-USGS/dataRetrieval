#' Function to return data from web services
#'
#' This function accepts a url parameter, and returns the raw data. 
#' 
#' To add a custom user agent, create an environmental variable: CUSTOM_DR_UA
#'
#' @param obs_url character containing the url for the retrieval
#' @param \dots information to pass to header request
#' @export
#' @return Returns xml, json, or text depending on the requested data.
#' @examplesIf is_dataRetrieval_user()
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- "00003"
#' property <- "00060"
#' obs_url <- constructNWISURL(siteNumber, property, startDate, endDate, "dv")
#' \donttest{
#' rawData <- getWebServiceData(obs_url)
#' }
getWebServiceData <- function(obs_url, ...) {
  
  if (!has_internet_2(obs_url)){
    message("No internet connection.")
    return(invisible(NULL))
  }
  
  obs_url <- httr2::req_user_agent(obs_url, default_ua())
  obs_url <- httr2::req_throttle(obs_url, rate = 30 / 60) 
  obs_url <- httr2::req_retry(obs_url,
                              backoff = ~ 5, max_tries = 3) 
  obs_url <- httr2::req_headers(obs_url,
                                `Accept-Encoding` = c("compress", "gzip")) 
  
  message("GET:", obs_url$url) 
  returnedList <- httr2::req_perform(obs_url)

  good <- check_non_200s(returnedList)
  
  return_readLines <- c("text/html", "text/html;charset=utf-8")

  return_content <- c("text/tab-separated-values;charset=utf-8",
                      "text/csv;charset=utf-8",
                      "text/csv",
                      "text/plain",
                      "text/plain;charset=utf-8")
  
  return_json <- c("application/vnd.geo+json;charset=utf-8")
  
  if(good){
    headerInfo <- httr2::resp_headers(returnedList)
    content <- gsub(" ", "", tolower(headerInfo$`content-type`))
    if (content %in% return_content) {
      returnedDoc <- httr2::resp_body_string(returnedList)
      trys <- 1
      if (all(grepl("ERROR: INCOMPLETE DATA", returnedDoc))) {
        
        while(trys <= 3){
          message("Trying again!")
          obs_url <- httr2::req_url_query(obs_url, 
                                          try = trys)
          returnedList <- httr2::req_perform(obs_url)
          
          good <- check_non_200s(returnedList)
          if(good){
            returnedDoc <- httr2::resp_body_string(returnedList)
          }
          if (all(grepl("ERROR: INCOMPLETE DATA", returnedDoc))) {
            trys <- trys + 1
          } else {
            trys <- 100
          }
        } 
      }

    # } else if (headerInfo$`content-type` %in% return_raw) {
    #   returnedDoc <- httr2::resp_body_raw(returnedList)
    } else if (content %in% return_readLines) {
      returnedList <- httr2::resp_body_string(returnedList)
      txt <- readLines(returnedList$content)
      message(txt)
      return(txt)
    } else if (content %in% return_json){
      returnedDoc <- httr2::resp_body_json(returnedList)
    } else {
      returnedDoc <- httr2::resp_body_xml(returnedList, encoding = "UTF-8")
      if (all(grepl("No sites/data found using the selection criteria specified", returnedDoc))) {
        message(returnedDoc)
      }
    }

    attr(returnedDoc, "headerInfo") <- headerInfo

    return(returnedDoc)
  } else {
    return(NULL)
  }
}

check_non_200s <- function(returnedList){
    
  status <- httr2::resp_status(returnedList)
  
  return(status == 200)
  

}

#' Create user agent
#'
#' @keywords internal
default_ua <- function() {
  versions <- c(
    libcurl = curl::curl_version()$version,
    httr2 = as.character(utils::packageVersion("httr2")),
    dataRetrieval = as.character(utils::packageVersion("dataRetrieval"))
  )

  ua <- paste0(names(versions), "/", versions, collapse = " ")

  if (Sys.getenv("CUSTOM_DR_UA") != "") {
    ua <- paste0(ua, "/", Sys.getenv("CUSTOM_DR_UA"))
  }

  return(ua)
}

#' has_internet_2
#'
#' Function to check for internet even if the user
#' is behind a proxy
#' 
#' If this is giving you problems, override the false negative:
#' Sys.setenv("OVERRIDE_INTERNET_TEST" = TRUE)
#'
#' @keywords internal
#' @param obs_url character obs_url to check
has_internet_2 <- function(obs_url) {
  
  if (nzchar(Sys.getenv("OVERRIDE_INTERNET_TEST"))) {
    if(Sys.getenv("OVERRIDE_INTERNET_TEST") == "FALSE"){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  
  if("url" %in% names(obs_url)){
    url <- obs_url$url
  } else {
    url <- obs_url
  }
  
  host <- gsub("^https://(?:www[.])?([^/]*).*$", "\\1", url)

  !is.null(curl::nslookup(host, error = FALSE))
}

#' getting header information from a WQP query
#'
#' @param url the query url
getQuerySummary <- function(url) {
  wqp_message()
  
  queryHEAD <- httr2::req_method(req = url ,
                                 method =  "HEAD")
  queryHEAD <- httr2::req_perform(queryHEAD)
  headerInfo <- httr2::resp_headers(queryHEAD)
  retquery <- data.frame(t(unlist(headerInfo)))
  names(retquery) <- gsub("\\.", "-", names(retquery))
  retquery[,grep("-count", names(retquery))] <- as.numeric(retquery[grep("-count", names(retquery))])

  if ("date" %in% names(retquery)) {
    retquery$date <- as.Date(retquery$date, format = "%a, %d %b %Y %H:%M:%S")
  }

  return(retquery)
}


