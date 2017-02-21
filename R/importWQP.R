

#' Basic Water Quality Portal Data parser
#'
#' Imports data from the Water Quality Portal based on a specified url.
#' 
#' @param obs_url character URL to Water Quality Portal#' @keywords data import USGS web service
#' @param zip logical to request data via downloading zip file. Default set to FALSE.
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally, a POSIXct dateTime column is supplied for 
#' start and end times, and converted to UTC. See \url{https://www.waterqualitydata.us/portal_userguide/} for more information.
#' @export
#' @seealso \code{\link{readWQPdata}}, \code{\link{readWQPqw}}, \code{\link{whatWQPsites}}
#' @import utils
#' @import stats
#' @importFrom readr read_delim
#' @importFrom readr col_character
#' @importFrom readr col_number
#' @importFrom readr cols
#' @importFrom dplyr mutate_
#' @importFrom dplyr mutate_each_
#' @importFrom dplyr select_
#' @importFrom dplyr left_join
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate fast_strptime
#' @importFrom httr GET
#' @importFrom httr user_agent
#' @importFrom httr write_disk
#' @examples
#' # These examples require an internet connection to run
#' 
#' ## Examples take longer than 5 seconds:
#' \dontrun{
#' rawSampleURL <- constructWQPURL('USGS-01594440','01075', '', '')
#' 
#' rawSample <- importWQP(rawSampleURL)
#' 
#' rawSampleURL_Zip <- constructWQPURL('USGS-01594440','01075', '', '', TRUE)
#' rawSample2 <- importWQP(rawSampleURL_Zip, zip=TRUE)
#' 
#' STORETex <- constructWQPURL('WIDNR_WQX-10032762','Specific conductance', '', '')
#' STORETdata <- importWQP(STORETex)
#' }
importWQP <- function(obs_url, zip=FALSE, tz=""){
  
  if(tz != ""){
    tz <- match.arg(tz, OlsonNames())
  } else {
    tz <- "UTC"
  }
  
  if(!file.exists(obs_url)){
    
    if(zip){
      message("zip encoding access still in development")
      temp <- tempfile()
      temp <- paste0(temp,".zip")
      doc <- getWebServiceData(obs_url, write_disk(temp))
      headerInfo <- headers(doc)
      doc <- unzip(temp, exdir=tempdir())
      unlink(temp)
      on.exit(unlink(doc))
    } else {
      doc <- getWebServiceData(obs_url)
      headerInfo <- attr(doc, "headerInfo")
    }
    
    numToBeReturned <- 0
    sitesToBeReturned <- 0
    
    if("total-result-count" %in% names(headerInfo)){
      numToBeReturned <- as.numeric(headerInfo["total-result-count"])
    } 
    
    if("total-site-count" %in% names(headerInfo)){
      sitesToBeReturned <- as.numeric(headerInfo["total-site-count"])
    }
    
    
    totalReturned <- sum(numToBeReturned, sitesToBeReturned,na.rm = TRUE)
    
    if(is.na(totalReturned) | totalReturned == 0){
      for(i in grep("Warning",names(headerInfo))){
        warning(headerInfo[i])
      }
      emptyReturn <- data.frame(NA)
      attr(emptyReturn, "headerInfo") <- headerInfo
      return(emptyReturn)
    }
    
  } else {
    if(zip){
      doc <- unzip(obs_url)
    } else {
      doc <- obs_url
    }
    
  }

  retval <- suppressWarnings(read_delim(doc, 
                       col_types = cols(`ActivityStartTime/Time` = col_character(),
                                        `ActivityEndTime/Time` = col_character(),
                                        USGSPCode = col_character(),
                                        ResultCommentText=col_character(),
                                        `ActivityDepthHeightMeasure/MeasureValue` = col_number(),
                                        `DetectionQuantitationLimitMeasure/MeasureValue` = col_number(),
                                        ResultMeasureValue = col_number(),
                                        `WellDepthMeasure/MeasureValue` = col_number(),
                                        `WellHoleDepthMeasure/MeasureValue` = col_number(),
                                        `HUCEightDigitCode` = col_character()),
                       quote = "", delim = "\t"))
    
  if(!file.exists(obs_url)){
    actualNumReturned <- nrow(retval)
    
    if(actualNumReturned != numToBeReturned & actualNumReturned != sitesToBeReturned){
      warning(totalReturned, " sample results were expected, ", actualNumReturned, " were returned")
    } 
  }
  
  if(length(grep("ActivityStartTime",names(retval))) > 0){
    

    offsetLibrary <- data.frame(offset=c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10, 0, 0),
                                code=c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST","", NA),
                                stringsAsFactors = FALSE)
    
    retval <- left_join(retval, offsetLibrary, by=c("ActivityStartTime/TimeZoneCode"="code"))
    names(retval)[names(retval) == "offset"] <- "timeZoneStart"
    retval <- left_join(retval, offsetLibrary, by=c("ActivityEndTime/TimeZoneCode"="code"))
    names(retval)[names(retval) == "offset"] <- "timeZoneEnd"
    
    dateCols <- c("ActivityStartDate","ActivityEndDate","AnalysisStartDate","PreparationStartDate")

    for(i in dateCols){
      retval[,i] <- suppressWarnings(as.Date(parse_date_time(retval[[i]], c("Ymd", "mdY"))))
    }
    

    retval <- mutate_(retval, ActivityStartDateTime=~paste(ActivityStartDate, `ActivityStartTime/Time`))
    retval <- mutate_(retval, ActivityEndDateTime=~paste(ActivityEndDate, `ActivityEndTime/Time`))
    
    retval <- mutate_(retval, ActivityStartDateTime=~fast_strptime(ActivityStartDateTime, '%Y-%m-%d %H:%M:%S')+60*60*timeZoneStart)
    retval <- mutate_(retval, ActivityEndDateTime=~fast_strptime(ActivityEndDateTime, '%Y-%m-%d %H:%M:%S')+60*60*timeZoneStart)
    
    retval <- select_(retval, ~-timeZoneEnd, ~-timeZoneStart)
  }
  names(retval)[grep("/",names(retval))] <- gsub("/",".",names(retval)[grep("/",names(retval))])
  
  return(retval)
  
  
}
