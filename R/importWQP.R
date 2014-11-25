#' Basic Water Quality Portal Data parser
#'
#' Imports data from the Water Quality Portal based on a specified url.
#' 
#' @param url character URL to Water Quality Portal#' @keywords data import USGS web service
#' @param zip logical used to request the data in a zip format (TRUE)
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally, a POSIXct dateTime column is supplied for 
#' start and end times.
#' @export
#' @seealso \code{\link{readWQPdata}}, \code{\link{readWQPqw}}, \code{\link{whatWQPsites}}
#' @import RCurl
#' @import httr
#' @import lubridate
#' @examples
#' # These examples require an internet connection to run
#' \dontrun{
#' ## Examples take longer than 5 seconds:
#' rawSampleURL <- constructWQPURL('USGS-01594440','01075', '', '')
#' rawSample <- importWQP(rawSampleURL)
#' url2 <- paste0(rawSampleURL,"&zip=yes")
#' rawSample2 <- importWQP(url2, TRUE)
#' STORETex <- constructWQPURL('WIDNR_WQX-10032762','Specific conductance', '', '')
#' STORETdata <- importWQP(STORETex)
#' }
importWQP <- function(url, zip=FALSE, tz=""){
  
  h <- basicHeaderGatherer()
  
  tryCatch({  
    if(zip){
      headerInfo <- HEAD(url)$headers
      temp <- tempfile()
      options(timeout = 120)
      download.file(url,temp, quiet=TRUE, mode='wb')
      doc <- unzip(temp)
      unlink(temp)
    } else {
      doc <- getURL(url, headerfunction = h$update)
      headerInfo <- h$value()
    
    }
  }, warning = function(w) {
    message(paste("URL caused a warning:", url))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", url))
    message(e)
    return(NA)
  })
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }
    
  numToBeReturned <- as.numeric(headerInfo["Total-Result-Count"])
  
  if (!is.na(numToBeReturned) | numToBeReturned != 0){

    suppressWarnings(namesData <- read.delim(if(zip) doc else textConnection(doc) , header = TRUE, quote="\"",
                                             dec=".", sep='\t',
                                             colClasses='character',
                                             fill = TRUE,nrow=1))
          
    classColumns <- setNames(rep('character',ncol(namesData)),names(namesData))
    
    classColumns[grep("MeasureValue",names(classColumns))] <- NA
    
    suppressWarnings(retval <- read.delim(if(zip) doc else textConnection(doc), header = TRUE, quote="\"", 
                         dec=".", sep='\t', 
                         colClasses=as.character(classColumns), 
                         fill = TRUE))
    
    actualNumReturned <- nrow(retval)
    
    retval[,names(which(sapply(retval[,grep("MeasureValue",names(retval))], function(x)all(is.na(x)))))] <- ""
    
    if(actualNumReturned != numToBeReturned) warning(numToBeReturned, " sample results were expected, ", actualNumReturned, " were returned")
    
    offsetLibrary <- setNames(c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10),
                              c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
    
    timeZoneStart <- offsetLibrary[retval$ActivityStartTime.TimeZoneCode]
    timeZoneEnd <- offsetLibrary[retval$ActivityEndTime.TimeZoneCode]
    timeZoneStart[is.na(timeZoneStart)] <- 0
    timeZoneEnd[is.na(timeZoneEnd)] <- 0
    
    if("ActivityStartDate" %in% names(retval)){
      if(any(retval$ActivityStartDate != "")){
        suppressWarnings(retval$ActivityStartDate <- as.Date(parse_date_time(retval$ActivityStartDate, c("Ymd", "mdY"))))
      }
    }

    if("ActivityEndDate" %in% names(retval)){
      if(any(retval$ActivityEndDate != "")){
        suppressWarnings(retval$ActivityEndDate <- as.Date(parse_date_time(retval$ActivityEndDate, c("Ymd", "mdY"))))
      }        
    }

    if(any(!is.na(timeZoneStart))){
      
      retval$ActivityStartDateTime <- with(retval, as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time),format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
      retval$ActivityStartDateTime <- retval$ActivityStartDateTime + timeZoneStart*60*60
      retval$ActivityStartDateTime <- as.POSIXct(retval$ActivityStartDateTime)
      
    }
    
    if(any(!is.na(timeZoneEnd))){      
      retval$ActivityEndDateTime <- with(retval, as.POSIXct(paste(ActivityEndDate, ActivityEndTime.Time),format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
      retval$ActivityEndDateTime <- retval$ActivityEndDateTime + timeZoneEnd*60*60
      retval$ActivityEndDateTime <- as.POSIXct(retval$ActivityEndDateTime)      
    }
    
    if(all(is.na(retval$ActivityEndDateTime))){
      retval$ActivityEndDateTime <- NULL
    }
            
    siteInfo <- whatWQPsites(siteid=paste(unique(retval$MonitoringLocationIdentifier),collapse=","))
    
    siteInfoCommon <- data.frame(station_nm=siteInfo$MonitoringLocationName,
                                 agency_cd=siteInfo$OrganizationIdentifier,
                                 site_no=siteInfo$MonitoringLocationIdentifier,
                                 dec_lat_va=siteInfo$LatitudeMeasure,
                                 dec_lon_va=siteInfo$LongitudeMeasure,
                                 hucCd=siteInfo$HUCEightDigitCode,
                                 stringsAsFactors=FALSE)
    
    siteInfo <- cbind(siteInfoCommon, siteInfo)
                                 
    
    variableInfo <- data.frame(characteristicName=retval$CharacteristicName,
                               parameterCd=retval$USGSPCode,
                               param_units=retval$ResultMeasure.MeasureUnitCode,
                               valueType=retval$ResultSampleFractionText,
                               stringsAsFactors=FALSE)
    variableInfo <- unique(variableInfo)
    
    attr(retval, "siteInfo") <- siteInfo
    attr(retval, "variableInfo") <- variableInfo
    
    return(retval)
    
  } else {
    warning("No data to retrieve")
    return(NA)
  }

}