
#' Function to return data from the NWIS RDB 1.0 format
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. 
#'
#' @param obs_url string containing the url for the retrieval
#' @param asDateTime logical, if TRUE returns date and time as POSIXct, if FALSE, Date
#' @param qw logical, if TRUE parses as water quality data (where dates/times are in start and end times)
#' @return data a data frame containing columns agency, site, dateTime, values, and remark codes for all requested combinations
#' @export
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- "00003"
#' property <- "00060"
#' obs_url <- constructNWISURL(siteNumber,property,
#'          startDate,endDate,"dv",format="tsv")
#' data <- importRDB1(obs_url)
#' urlMulti <- constructNWISURL("04085427",c("00060","00010"),
#'          startDate,endDate,"dv",statCd=c("00003","00001"),"tsv")
#' multiData <- importRDB1(urlMulti)
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          "2014-10-10","2014-10-10","uv",format="tsv")
#' unitData <- importRDB1(unitDataURL, asDateTime=TRUE)
importRDB1 <- function(obs_url,asDateTime=FALSE, qw=FALSE){
  
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
  
  if(as.character(h$value()["Content-Type"]) == "text/plain;charset=UTF-8" | as.character(h$value()["Content-Type"]) == "text/plain"){
    
#     comments <- readLines(doc)
    
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
    
    timeZoneLibrary <- setNames(c("America/New_York","America/New_York","America/Chicago","America/Chicago",
                                  "America/Denver","America/Denver","America/Los_Angeles","America/Los_Angeles",
                                  "America/Anchorage","America/Anchorage","America/Honolulu","America/Honolulu"),
                                c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
    
    data[,grep('n$', dataType)] <- suppressWarnings(sapply(data[,grep('n$', dataType)], function(x) as.numeric(x)))
    
    if(length(grep('d$', dataType)) > 0){
      if (asDateTime & !qw){
        
        if("tz_cd" %in% names(data)){
          timeZone <- as.character(timeZoneLibrary[data$tz_cd])
        } else {
          timeZone <- NULL
        }
        
        
        if(length(unique(timeZone)) == 1){
          data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = unique(timeZone))
        } else {
          
          mostCommonTZ <- names(sort(summary(as.factor(timeZone)),decreasing = TRUE)[1])

          data[,grep('d$', dataType)] <- as.POSIXct(data[,grep('d$', dataType)], "%Y-%m-%d %H:%M", tz = mostCommonTZ)
          additionalTZs <- names(sort(summary(as.factor(timeZone)),decreasing = TRUE)[-1])
          for(i in additionalTZs){
            data[timeZone == i,grep('d$', dataType)] <-  as.POSIXct(data[,grep('d$', dataType)], "%Y-%m-%d %H:%M", tz = i)
          }
        }
       
      } else if (qw){
        
        if("sample_start_time_datum_cd" %in% names(data)){
          timeZoneStart <- as.character(timeZoneLibrary[data$sample_start_time_datum_cd])
        } else {
          timeZoneStart <- NA
        }
        
        if("sample_end_time_datum_cd" %in% names(data)){
          timeZoneEnd <- as.character(timeZoneLibrary[data$sample_end_time_datum_cd])
        } else {
          timeZoneEnd <- NA
        }
        timeZoneStart[is.na(timeZoneStart)] <- ""
        timeZoneEnd[is.na(timeZoneEnd)] <- ""
        
        if("sample_dt" %in% names(data)){
          if(any(data$sample_dt != "")){
            suppressWarnings(data$sample_dt <- as.Date(parse_date_time(data$sample_dt, c("Ymd", "mdY"))))
          }
        }
        
        if("sample_end_dt" %in% names(data)){
          if(any(data$sample_end_dt != "")){
            suppressWarnings(data$sample_end_dt <- as.Date(parse_date_time(data$sample_end_dt, c("Ymd", "mdY"))))
          }        
        }
        
        if(any(!is.na(timeZoneStart))){
          if(length(unique(timeZoneStart)) == 1){
            data$startDateTime <- with(data, as.POSIXct(paste(sample_dt, sample_tm),format="%Y-%m-%d %H:%M", tz=unique(timeZoneStart)))
          } else {
            
            mostCommonTZ <- names(sort(summary(as.factor(timeZoneStart)),decreasing = TRUE)[1])
            
            data$startDateTime <- with(data, as.POSIXct(paste(sample_dt, sample_tm),
                                            format="%Y-%m-%d %H:%M", 
                                            tz=mostCommonTZ))
            additionalTZs <- names(sort(summary(as.factor(timeZoneStart)),decreasing = TRUE)[-1])
            for(i in additionalTZs){
              data$startDateTime[timeZoneStart == i] <-  with(data[timeZoneStart == i,], 
                                  as.POSIXct(paste(sample_dt, sample_tm),
                                             format="%Y-%m-%d %H:%M", 
                                             tz=i))
            }
          }
        }
        
        if(any(!is.na(timeZoneEnd))){
          if(length(unique(timeZoneEnd)) == 1){
            data$endDateTime <- with(data, as.POSIXct(paste(sample_end_dt, sample_end_tm),format="%Y-%m-%d %H:%M", tz=unique(timeZoneEnd)))
          } else {
            
            mostCommonTZ <- names(sort(summary(as.factor(timeZoneEnd)),decreasing = TRUE)[1])
            
            data$endDateTime <- with(data, as.POSIXct(paste(sample_end_dt, sample_end_tm),
                                format="%Y-%m-%d %H:%M", 
                                tz=mostCommonTZ))
            additionalTZs <- names(sort(summary(as.factor(timeZoneEnd)),decreasing = TRUE)[-1])
            for(i in additionalTZs){
              data$endDateTime[timeZoneEnd == i] <-  with(data[timeZoneStart == i,], 
                                as.POSIXct(paste(sample_end_dt, sample_end_tm),
                                           format="%Y-%m-%d %H:%M", 
                                           tz=i))
            }
          }
        }
        
      } else {
        for (i in grep('d$', dataType)){
          if (all(data[,i] != "")){
            data[,i] <- as.Date(data[,i])
          }
          
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
