#' Basic Water Quality Portal Data parser
#'
#' Imports data from the Water Quality Portal based on a specified url.
#' 
#' @param obs_url character URL to Water Quality Portal#' @keywords data import USGS web service
#' @param zip logical used to request the data in a zip format (TRUE)
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally, a POSIXct dateTime column is supplied for 
#' start and end times, and converted to UTC. See \url{http://www.waterqualitydata.us/portal_userguide.jsp} for more information.
#' @export
#' @seealso \code{\link{readWQPdata}}, \code{\link{readWQPqw}}, \code{\link{whatWQPsites}}
#' @import RCurl
#' @import lubridate
#' @examples
#' # These examples require an internet connection to run
#' 
#' ## Examples take longer than 5 seconds:
#' \dontrun{
#' rawSampleURL <- constructWQPURL('USGS-01594440','01075', '', '')
#' 
#' rawSample <- importWQP(rawSampleURL)
#' url2 <- paste0(rawSampleURL,"&zip=yes")
#' rawSample2 <- importWQP(url2, TRUE)
#' 
#' STORETex <- constructWQPURL('WIDNR_WQX-10032762','Specific conductance', '', '')
#' STORETdata <- importWQP(STORETex)
#' }
importWQP <- function(obs_url, zip=FALSE, tz=""){
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }
  
  if(zip){
    h <- basicHeaderGatherer()
    httpHEAD(obs_url, headerfunction = h$update)
    
    headerInfo <- h$value()
    
    temp <- tempfile()
    options(timeout = 120)
    
    possibleError <- tryCatch({
      download.file(obs_url,temp, quiet=TRUE, mode='wb')
      },
      error = function(e)  {
        stop(e, "with url:", obs_url)
      }
    )
    
    if(headerInfo['status'] == "200"){
      doc <- unzip(temp)
    } else {
      unlink(temp)

      stop("Status:", headerInfo['status'], ": ", headerInfo['statusMessage'], "\nFor: ", obs_url)
    }
    
  } else {
    doc <- getWebServiceData(obs_url)
    headerInfo <- attr(doc, "headerInfo")
  }
    
  numToBeReturned <- as.numeric(headerInfo["Total-Result-Count"])
  
  if (!is.na(numToBeReturned) & numToBeReturned != 0){

    suppressWarnings(namesData <- read.delim(if(zip) doc else textConnection(doc) , header = TRUE, quote="",
                                             dec=".", sep='\t', colClasses='character',nrow=1))
          
    classColumns <- setNames(rep('character',ncol(namesData)),names(namesData))
    
    classColumns[grep("MeasureValue",names(classColumns))] <- NA
    
    suppressWarnings(retval <- read.delim(if(zip) doc else textConnection(doc), header = TRUE, quote="", 
                         dec=".", sep='\t', colClasses=as.character(classColumns)))
    
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
      if(tz != ""){
        attr(retval$ActivityStartDateTime, "tzone") <- tz
      } else {
        attr(retval$ActivityStartDateTime, "tzone") <- "UTC"
      }      
    }
    
    if(any(!is.na(timeZoneEnd))){      
      retval$ActivityEndDateTime <- with(retval, as.POSIXct(paste(ActivityEndDate, ActivityEndTime.Time),format="%Y-%m-%d %H:%M:%S", tz = "UTC"))
      retval$ActivityEndDateTime <- retval$ActivityEndDateTime + timeZoneEnd*60*60
      retval$ActivityEndDateTime <- as.POSIXct(retval$ActivityEndDateTime)
      if(tz != ""){
        attr(retval$ActivityEndDateTime, "tzone") <- tz
      } else {
        attr(retval$ActivityEndDateTime, "tzone") <- "UTC"
      }
    }
    
    if(all(is.na(retval$ActivityEndDateTime))){
      retval$ActivityEndDateTime <- NULL
    }
    if(zip){
      unlink(doc)
    }
    
    return(retval)
  
  } else {
    
    if(headerInfo['Total-Result-Count'] == "0"){
      warning("No data returned")
    }
    
    for(i in grep("Warning",names(headerInfo))){
      warning(headerInfo[i])
    }
    
    
  }

}