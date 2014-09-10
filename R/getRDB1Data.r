#' Function to return data from the NWIS RDB 1.0 format
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url string containing the url for the retrieval
#' @param asDateTime logical, if TRUE returns date and time as POSIXct, if FALSE, Date
#' @return data a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- '00003'
#' property <- '00060'
#' obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv',format='tsv')
#' data <- getRDB1Data(obs_url)
#' urlMulti <- constructNWISURL("04085427",c("00060","00010"),startDate,endDate,'dv',statCd=c("00003","00001"),'tsv')
#' multiData <- getRDB1Data(urlMulti)
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          as.character(Sys.Date()),as.character(Sys.Date()),'uv',format='tsv')
#' unitData <- getRDB1Data(unitDataURL, asDateT=TRUE)
getRDB1Data <- function(obs_url,asDateTime=FALSE){
  
  retval = tryCatch({
    h <- basicHeaderGatherer()
    doc <- getURL(obs_url, headerfunction = h$update)
    
  }, warning = function(w) {
    message(paste("URL caused a warning:", obs_url))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", obs_url))
    message(e)
    return(NA)
  })   
  
  if(as.character(h$value()["Content-Type"]) == "text/plain;charset=UTF-8"){
    
    tmp <- read.delim(  
      textConnection(doc), 
      header = TRUE, 
      quote="\"", 
      dec=".", 
      sep='\t',
      colClasses=c('character'),
      fill = TRUE, 
      comment.char="#")
    
    dataType <- tmp[1,]
    data <- tmp[-1,]
    
    if(sum(regexpr('d$', dataType) > 0) > 0){
      if (asDateTime){
        
        timeZoneLibrary <- setNames(c("America/New_York","America/New_York","America/Chicago","America/Chicago",
                                      "America/Denver","America/Denver","America/Los_Angeles","America/Los_Angeles",
                                      "America/Anchorage","America/Anchorage","America/Honolulu","America/Honolulu"),
                                    c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
        timeZone <- as.character(timeZoneLibrary[data$tz_cd])
        if(length(unique(timeZone)) == 1){
          data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = unique(timeZone))
        } else {
          warning("Mixed time zone information")
          for(i in seq_along(row.names(data))){
            data[i,regexpr('d$', dataType) > 0] <- as.POSIXct(data[i,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = timeZone[i])
          }
        }
        
      } else {
        data[,regexpr('d$', dataType) > 0] <- as.Date(data[,regexpr('d$', dataType) > 0])
      }
    }
    
    if (sum(regexpr('n$', dataType) > 0) > 0){
      tempDF <- data[,which(regexpr('n$', dataType) > 0)]
      tempDF <- suppressWarnings(sapply(tempDF, function(x) as.numeric(x)))  
      data[,which(regexpr('n$', dataType) > 0)] <- tempDF
    }
    row.names(data) <- NULL
    return(data)
  } else {
    message(paste("URL caused a warning:", obs_url))
    message("Content-Type=",h$value()["Content-Type"])
  }
}
