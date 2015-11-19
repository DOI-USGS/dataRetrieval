

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
#' @import utils
#' @import stats
#' @importFrom readr read_delim
#' @importFrom readr col_character
#' @importFrom readr cols
#' @importFrom dplyr mutate_
#' @importFrom dplyr mutate_each_
#' @importFrom dplyr select_
#' @importFrom dplyr left_join
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate fast_strptime
#' @examples
#' # These examples require an internet connection to run
#' 
#' ## Examples take longer than 5 seconds:
#' \dontrun{
#' rawSampleURL <- constructWQPURL('USGS-01594440','01075', '', '')
#' 
#' rawSample <- importWQP(rawSampleURL)
#' url2 <- paste0(rawSampleURL,"&zip=yes")
#' rawSample2 <- importWQP(url2, zip=TRUE)
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
  
  if(!file.exists(obs_url)){
    h <- basicHeaderGatherer()
    httpHEAD(obs_url, headerfunction = h$update)
    
    headerInfo <- h$value()
    numToBeReturned <- as.numeric(headerInfo["Total-Result-Count"])
    
    if(headerInfo['Total-Result-Count'] == "0"){
      warning("No data returned")
      return(data.frame())
    }
    
    if(is.na(numToBeReturned) | numToBeReturned == 0){
      for(i in grep("Warning",names(headerInfo))){
        warning(headerInfo[i])
      }
      return(data.frame())
    }
    
    if(headerInfo['status'] == "200"){
      
      if(zip){
        temp <- tempfile()
        options(timeout = 120)
        
        possibleError <- tryCatch({
            suppressWarnings(download.file(obs_url,temp, quiet=TRUE, mode='wb'))
          },
          error = function(e)  {
            stop(e, "with url:", obs_url)
          }
        )
        doc <- unzip(temp)
        retval <- read_delim(doc, 
                             col_types = cols(`ActivityStartTime/Time` = col_character(),
                                              `ActivityEndTime/Time` = col_character(),
                                              USGSPCode = col_character(),
                                              ResultCommentText=col_character()),
                             quote = "", delim = "\t")
        unlink(doc)
      } else {
        retval <- read_delim(obs_url, 
                             col_types = cols(`ActivityStartTime/Time` = col_character(),
                                              `ActivityEndTime/Time` = col_character(),
                                              USGSPCode = col_character(),
                                              ResultCommentText=col_character()),
                             quote = "", delim = "\t")
      }
    } else {
      stop("Status:", headerInfo['status'], ": ", headerInfo['statusMessage'], "\nFor: ", obs_url)
    }
    
    actualNumReturned <- nrow(retval)
    if(actualNumReturned != numToBeReturned) warning(numToBeReturned, " sample results were expected, ", actualNumReturned, " were returned")
    
  } else {
    
    if(zip){
      doc <- unzip(obs_url)
      retval <- read_delim(obs_url, 
                           col_types = cols(`ActivityStartTime/Time` = col_character(),
                                            `ActivityEndTime/Time` = col_character(),
                                            USGSPCode = col_character(),
                                            ResultCommentText=col_character()),
                           quote = "", delim = "\t")
      unlink(doc)
    }  else {
      retval <- read_delim(obs_url, 
                           col_types = cols(`ActivityStartTime/Time` = col_character(),
                                            `ActivityEndTime/Time` = col_character(),
                                            USGSPCode = col_character(),
                                            ResultCommentText=col_character()),
                           quote = "", delim = "\t")
    }
    
  }
  
  offsetLibrary <- data.frame(offset=c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10, 0, 0),
                              code=c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST","", NA),
                              stringsAsFactors = FALSE)
  
  retval <- left_join(retval, offsetLibrary, by=c("ActivityStartTime/TimeZoneCode"="code"))
  names(retval)[names(retval) == "offset"] <- "timeZoneStart"
  retval <- left_join(retval, offsetLibrary, by=c("ActivityEndTime/TimeZoneCode"="code"))
  names(retval)[names(retval) == "offset"] <- "timeZoneEnd"
  
  dateCols <- c("ActivityStartDate","ActivityEndDate","AnalysisStartDate","PreparationStartDate")
  
  retval <- suppressWarnings(mutate_each_(retval, ~as.Date(parse_date_time(., c("Ymd", "mdY"))), dateCols))
  
  retval <- mutate_(retval, ActivityStartDateTime=~paste(ActivityStartDate, `ActivityStartTime/Time`))
  retval <- mutate_(retval, ActivityEndDateTime=~paste(ActivityEndDate, `ActivityEndTime/Time`))
  
  retval <- mutate_(retval, ActivityStartDateTime=~fast_strptime(ActivityStartDateTime, '%Y-%m-%d %H:%M:%S')+60*60*timeZoneStart)
  retval <- mutate_(retval, ActivityEndDateTime=~fast_strptime(ActivityEndDateTime, '%Y-%m-%d %H:%M:%S')+60*60*timeZoneStart)
  
  retval <- select_(retval, ~-timeZoneEnd, ~-timeZoneStart)
  names(retval)[grep("/",names(retval))] <- gsub("/",".",names(retval)[grep("/",names(retval))])
  
  return(retval)
  
  
}
