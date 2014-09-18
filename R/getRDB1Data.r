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
#' obs_url <- constructNWISURL(siteNumber,property,
#'          startDate,endDate,'dv',format='tsv')
#' data <- getRDB1Data(obs_url)
#' urlMulti <- constructNWISURL("04085427",c("00060","00010"),
#'          startDate,endDate,'dv',statCd=c("00003","00001"),'tsv')
#' multiData <- getRDB1Data(urlMulti)
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          as.character(Sys.Date()),as.character(Sys.Date()),'uv',format='tsv')
#' unitData <- getRDB1Data(unitDataURL, asDateTime=TRUE)
#' mulitSites <- getRDB1Data("http://waterservices.usgs.gov/nwis/dv/?format=rdb&stateCd=OH&parameterCd=00010")
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
    
    multiSiteCorrections <- -which(as.logical(apply(data[,1:2], 1, FUN=function(x) all(x %in% as.character(dataType[,1:2])))))
    if(length(multiSiteCorrections) > 0){
      data <- data[multiSiteCorrections,]
      
      findRowsWithHeaderInfo <- as.integer(apply(data[,1:2], 1, FUN = function(x) if(x[1] == names(data)[1] & x[2] == names(data)[2]) 1 else 0))
      findRowsWithHeaderInfo <- which(findRowsWithHeaderInfo == 0)
      data <- data[findRowsWithHeaderInfo,]
      
    }
    
    data[,grep('n$', dataType)] <- suppressWarnings(sapply(data[,grep('n$', dataType)], function(x) as.numeric(x)))
    
    if(length(grep('d$', dataType)) > 0){
      if (asDateTime){
        
        timeZoneLibrary <- setNames(c("America/New_York","America/New_York","America/Chicago","America/Chicago",
                                      "America/Denver","America/Denver","America/Los_Angeles","America/Los_Angeles",
                                      "America/Anchorage","America/Anchorage","America/Honolulu","America/Honolulu"),
                                    c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
        timeZone <- as.character(timeZoneLibrary[data$tz_cd])
        if(length(unique(timeZone)) == 1){
          data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = unique(timeZone))
        } else {
          
          mostCommonTZ <- names(sort(summary(as.factor(timeZone)),decreasing = TRUE)[1])

          data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = mostCommonTZ)
          additionalTZs <- names(sort(summary(as.factor(timeZone)),decreasing = TRUE)[-1])
          for(i in additionalTZs){
            data[timeZone == i,regexpr('d$', dataType) > 0] <-  as.POSIXct(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = i)
          }
        }
        
      } else {
        for (i in grep('d$', dataType)){
          data[,i] <- as.Date(data[,i])
        }
        
        
      }
    }
    
    row.names(data) <- NULL
    return(data)
  } else {
    message(paste("URL caused an error:", obs_url))
    message("Content-Type=",h$value()["Content-Type"])
  }
}
