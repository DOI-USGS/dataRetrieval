
#' Function to return data from the NWIS RDB 1.0 format
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. It is not
#' recommended to use the RDB format for importing multi-site data. 
#'
#' @param obs_url string containing the url for the retrieval
#' @param asDateTime logical, if TRUE returns date and time as POSIXct, if FALSE, Date
#' @param qw logical, if TRUE parses as water quality data (where dates/times are in start and end times)
#' @param tz string to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @param convertType logical, defaults to TRUE. If TRUE, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a string.
#' @return data a data frame containing columns agency, site, dateTime (converted to UTC), values, and remark codes for all requested combinations
#' @export
#' @import RCurl
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- "00003"
#' property <- "00060"
#' obs_url <- constructNWISURL(siteNumber,property,
#'          startDate,endDate,"dv",format="tsv")
#' data <- importRDB1(obs_url)
#' urlMultiPcodes <- constructNWISURL("04085427",c("00060","00010"),
#'          startDate,endDate,"dv",statCd=c("00003","00001"),"tsv")
#' multiData <- importRDB1(urlMultiPcodes)
#' unitDataURL <- constructNWISURL(siteNumber,property,
#'          "2013-11-03","2013-11-03","uv",format="tsv") #includes timezone switch
#' unitData <- importRDB1(unitDataURL, asDateTime=TRUE)
#' qwURL <- constructNWISURL(c('04024430','04024000'),
#'           c('34247','30234','32104','34220'),
#'          "2010-11-03","","qw",format="rdb") 
#' qwData <- importRDB1(qwURL, qw=TRUE, tz="America/Chicago")
#' iceSite <- '04024430'
#' start <- "2013-11-09"
#' end <- "2013-11-28"
#' urlIce <- constructNWISURL(iceSite,"00060",start, end,"uv",format="tsv")
#' 
#' # User file:
#' filePath <- system.file("extdata", package="dataRetrievaldemo")
#' fileName <- "RDB1Example.txt"
#' fullPath <- file.path(filePath, fileName)
#' importUserRDB <- importRDB1(fullPath)
importRDB1 <- function(obs_url, asDateTime=FALSE, qw=FALSE, convertType = TRUE, tz=""){
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }
  
  if(url.exists(obs_url)){
    
    # 400 bad site id
    # 404 outside date range, wrong pcode
    # 200 cool
    
    
    retval = tryCatch({
      h <- basicHeaderGatherer()
      doc <- getURL(obs_url, headerfunction = h$update)

      fileVecChar <- scan(obs_url, what = "", sep = "\n", quiet=TRUE)
      pndIndx<-regexpr("^#", fileVecChar)
      hdr <- fileVecChar[pndIndx > 0L]
      
      if(!(as.character(h$value()["Content-Type"]) == "text/plain;charset=UTF-8" | 
           as.character(h$value()["Content-Type"]) == "text/plain")){
        message(paste("URL caused an error:", obs_url))
        message("Content-Type=",h$value()["Content-Type"])
      }
      doc <- textConnection(doc)
      
    }, warning = function(w) {
      message(paste("URL caused a warning:", obs_url))
      message(w)
    }, error = function(e) {
      message(paste("URL does not seem to exist:", obs_url))
      message(e)
      return(NA)
    })
  } else {
    doc <- obs_url
    fileVecChar <- scan(obs_url, what = "", sep = "\n", quiet=TRUE)
    pndIndx<-regexpr("^#", fileVecChar)
    hdr <- fileVecChar[pndIndx > 0L]
  }
  
  tmp <- read.delim(  
    doc, 
    header = TRUE, 
    quote="\"", 
    dec=".", 
    sep='\t',
    colClasses=c('character'),
    fill = TRUE, 
    comment.char="#")
  
  dataType <- tmp[1,]
  data <- tmp[-1,]
  
  if(convertType){
    
    #This will break if the 2nd (or greater) site has more columns than the first
    #Therefore, using RDB is not recommended for multi-site queries.
    #This correction will work if each site has the same number of columns
    multiSiteCorrections <- -which(as.logical(apply(data[,1:2], 1, FUN=function(x) all(x %in% as.character(dataType[,1:2])))))
    
    if(length(multiSiteCorrections) > 0){
      data <- data[multiSiteCorrections,]
      
      findRowsWithHeaderInfo <- as.integer(apply(data[,1:2], 1, FUN = function(x) if(x[1] == names(data)[1] & x[2] == names(data)[2]) 1 else 0))
      findRowsWithHeaderInfo <- which(findRowsWithHeaderInfo == 0)
      data <- data[findRowsWithHeaderInfo,]
    }
    
    offsetLibrary <- setNames(c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10),
                                c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
    
    # The suppressed warning occurs when there is text (such as ice) in the numeric coluym
    data[,grep('n$', dataType)] <- suppressWarnings(sapply(data[,grep('n$', dataType)], function(x) as.numeric(x)))
    
    numberColumns <- grep("_va",names(data))    
    data[,numberColumns] <- sapply(data[,numberColumns],as.numeric)
    
    intColumns <- grep("_nu",names(data))
    
    if("current_rating_nu" %in% names(data)){
      intColumns <- intColumns[!("current_rating_nu" %in% names(data)[intColumns])]
      data$current_rating_nu <- gsub(" ", "", data$current_rating_nu)
    }
    data[,intColumns] <- sapply(data[,intColumns],as.integer)
    
    if(length(grep('d$', dataType)) > 0){
      if (asDateTime & !qw){
        
        if("tz_cd" %in% names(data)){
          offset <- offsetLibrary[data$tz_cd]
        } else {
          offset <- 0
        }
        offset[is.na(offset)] <- 0
        
        data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = "UTC")
        data[,regexpr('d$', dataType) > 0] <- data[,regexpr('d$', dataType) > 0] + offset*60*60
        data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0])
        
        if(tz != ""){
          attr(data[,regexpr('d$', dataType) > 0], "tzone") <- tz
        }
       
      } else if (qw){
        
        if("sample_start_time_datum_cd" %in% names(data)){
          timeZoneStartOffset <- offsetLibrary[data$sample_start_time_datum_cd]
          timeZoneStartOffset[is.na(timeZoneStartOffset)] <- 0
        } else {
          timeZoneStartOffset <- 0
        }
        
        if("sample_end_time_datum_cd" %in% names(data)){
          timeZoneEndOffset <- offsetLibrary[data$sample_end_time_datum_cd]
          timeZoneEndOffset[is.na(timeZoneEndOffset)] <- 0
          composite <- TRUE
        } else {
          composite <- FALSE
          if(any(data$sample_end_dt != "") & any(data$sample_end_dm != "")){
            if(which(data$sample_end_dt != "") == which(data$sample_end_dm != "")){
              composite <- TRUE
            }
          }
          timeZoneEndOffset <- 0
        }
        
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
        
        data$startDateTime <- with(data, as.POSIXct(paste(sample_dt, sample_tm),format="%Y-%m-%d %H:%M", tz = "UTC"))
        data$startDateTime <- data$startDateTime + timeZoneStartOffset*60*60
        data$startDateTime <- as.POSIXct(data$startDateTime)
        
        if(tz != ""){
          attr(data$startDateTime, "tzone") <- tz
        }
        
        if(composite){
          data$endDateTime <- with(data, as.POSIXct(paste(sample_end_dt, sample_end_tm),format="%Y-%m-%d %H:%M", tz = "UTC"))
          data$endDateTime <- data$endDateTime + timeZoneEndOffset*60*60
          data$endDateTime <- as.POSIXct(data$endDateTime)
          
          if(tz != ""){
            attr(data$endDateTime, "tzone") <- tz
          }
        }
        
      } else {
        for (i in grep('d$', dataType)){
          if (all(data[,i] != "")){
            data[,i] <- as.character(data[,i])
          }
        }
      }
    }
  
    row.names(data) <- NULL
  }
  
  comment(data) <- hdr
  
  return(data)

}
