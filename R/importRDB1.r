
#' Function to return data from the NWIS RDB 1.0 format
#'
#' This function accepts a url parameter that already contains the desired
#' NWIS site, parameter code, statistic, startdate and enddate. It is not
#' recommended to use the RDB format for importing multi-site data. 
#'
#' @param obs_url character containing the url for the retrieval or a file path to the data file.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param qw logical, if \code{TRUE} parses as water quality data (where dates/times are in start and end times)
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' datetime \tab POSIXct \tab The date and time of the value converted to UTC (if asDateTime = \code{TRUE}), \cr 
#' \tab character \tab or raw character string (if asDateTime = FALSE) \cr
#' tz_cd \tab character \tab The time zone code for datetime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form 
#' XD_P_S, where X is literal, 
#' D is an option description of the parameter, 
#' P is the parameter code, 
#' and S is the statistic code (if applicable).
#' If a date/time (dt) column contained incomplete date and times, a new column of dates was inserted. This could happen
#' when older data was reported as dates, and newer data was reported as a date/time.
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' }
#' @export
#' @import RCurl
#' @import utils
#' @import stats
#' @importFrom dplyr left_join
#' @examples
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- "00003"
#' property <- "00060"
#' 
#' obs_url <- constructNWISURL(siteNumber,property,
#'          startDate,endDate,"dv",format="tsv")
#' \dontrun{
#' data <- importRDB1(obs_url)
#' 
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
#' iceSite <- '04024000'
#' start <- "2014-11-09"
#' end <- "2014-11-28"
#' urlIce <- constructNWISURL(iceSite,"00060",start, end,"uv",format="tsv")
#' ice <- importRDB1(urlIce, asDateTime=TRUE)
#' iceNoConvert <- importRDB1(urlIce, convertType=FALSE)
#' }
#' # User file:
#' filePath <- system.file("extdata", package="dataRetrieval")
#' fileName <- "RDB1Example.txt"
#' fullPath <- file.path(filePath, fileName)
#' importUserRDB <- importRDB1(fullPath)
#' 
importRDB1 <- function(obs_url, asDateTime=FALSE, qw=FALSE, convertType = TRUE, tz=""){
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }
  

  if(file.exists(obs_url)){
    doc <- obs_url
  } else {
    rawData <- getWebServiceData(obs_url)
    doc <- textConnection(rawData)
    if("warn" %in% names(attr(rawData,"header"))){
      data <- data.frame()
      attr(data, "header") <- attr(rawData,"header")
      attr(data, "url") <- obs_url
      attr(data, "queryTime") <- Sys.time()
      
      return(data)
    }
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
  
  fileVecChar <- scan(obs_url, what = "", sep = "\n", quiet=TRUE)
  pndIndx<-regexpr("^#", fileVecChar)
  hdr <- fileVecChar[pndIndx > 0L]
  
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
    
    offsetLibrary <- data.frame(offset=c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10),
                                tz_cd=c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"),
                                stringsAsFactors = FALSE)

    # The suppressed warning occurs when there is text (such as ice) in the numeric column:
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
          offset <- left_join(data[,"tz_cd",drop=FALSE],offsetLibrary, by="tz_cd")
          offset <- offset$offset
          offset[is.na(offset)] <- median(offset, na.rm=TRUE)
        } else {
          offset <- 0
        }
        # offset[is.na(offset)] <- 0
        rawDateTimes <- data[,regexpr('d$', dataType) > 0]
        data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0], "%Y-%m-%d %H:%M", tz = "UTC")

        
        if(any(is.na(data[,regexpr('d$', dataType) > 0]))){
          data[,paste(names(data)[regexpr('d$', dataType) > 0],"date",sep = "_")] <- as.Date(substr(rawDateTimes,1,10))
        }

        data[,regexpr('d$', dataType) > 0] <- data[,regexpr('d$', dataType) > 0] + offset*60*60
        data[,regexpr('d$', dataType) > 0] <- as.POSIXct(data[,regexpr('d$', dataType) > 0])
                
        if(tz != ""){
          attr(data[,regexpr('d$', dataType) > 0], "tzone") <- tz
          data$tz_cd <- rep(tz, nrow(data))
        } else {
          attr(data[,regexpr('d$', dataType) > 0], "tzone") <- "UTC"
          data$tz_cd[!is.na(data[,regexpr('d$', dataType) > 0])] <- "UTC"
        }   
       
      } else if (qw){
        
        if("sample_start_time_datum_cd" %in% names(data)){
          timeZoneStartOffset <- left_join(data[,"sample_start_time_datum_cd",drop=FALSE],offsetLibrary, 
                                           by=c("sample_start_time_datum_cd"="tz_cd"))
          timeZoneStartOffset <- timeZoneStartOffset$offset
          timeZoneStartOffset[is.na(timeZoneStartOffset)] <- 0
        } else {
          timeZoneStartOffset <- 0
        }
        
        composite <- "sample_end_time_datum_cd" %in% names(data)
        if(composite){
          timeZoneEndOffset <- left_join(data[,"sample_end_time_datum_cd",drop=FALSE],offsetLibrary, 
                                           by=c("sample_end_time_datum_cd"="tz_cd"))
          timeZoneEndOffset <- timeZoneEndOffset$offset
          timeZoneEndOffset[is.na(timeZoneEndOffset)] <- 0
        } else {
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
          data$tz_cd <- rep(tz, nrow(data))
        } else {
          attr(data$startDateTime, "tzone") <- "UTC"
          data$tz_cd[!is.na(data$startDateTime)] <- "UTC"
        }        
        
        if(composite){
          data$endDateTime <- with(data, as.POSIXct(paste(sample_end_dt, sample_end_tm),format="%Y-%m-%d %H:%M", tz = "UTC"))
          data$endDateTime <- data$endDateTime + timeZoneEndOffset*60*60
          data$endDateTime <- as.POSIXct(data$endDateTime)
          
          if(tz != ""){
            attr(data$endDateTime, "tzone") <- tz
          } else {
            attr(data$endDateTime, "tzone") <- "UTC"
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
  
  names(data) <- make.names(names(data))
  
  comment(data) <- hdr
  attr(data, "url") <- obs_url
  attr(data, "queryTime") <- Sys.time()
  if(!file.exists(obs_url)){
    attr(data, "header") <- attr(rawData, "header")
  }
  
  return(data)
  

}
