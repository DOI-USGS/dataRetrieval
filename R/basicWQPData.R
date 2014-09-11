#' Basic Water Quality Portal Data grabber
#'
#' Imports data from the Water Quality Portal based on a specified url.
#' 
#' @param url string URL to Water Quality Portal#' @keywords data import USGS web service
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally, a POSIXct dateTime column is supplied for 
#' start and end times.
#' @export
#' @import RCurl
#' @examples
#' # These examples require an internet connection to run
#' rawSampleURL <- constructNWISURL('USGS-01594440','01075', '1985-01-01', '1985-03-31',"wqp")
#' rawSample <- basicWQPData(rawSampleURL)
basicWQPData <- function(url){
  
  h <- basicHeaderGatherer()
  
  retval = tryCatch({  
    doc <- getURL(url, headerfunction = h$update)
    
  }, warning = function(w) {
    message(paste("URL caused a warning:", url))
    message(w)
  }, error = function(e) {
    message(paste("URL does not seem to exist:", url))
    message(e)
    return(NA)
  })
  
  if(h$value()["Content-Type"] == "text/tab-separated-values;charset=UTF-8"){
    
    numToBeReturned <- as.numeric(h$value()["Total-Result-Count"])
    
    if (!is.na(numToBeReturned) | numToBeReturned != 0){
      
      
      namesData <- read.delim(textConnection(doc), header = TRUE, quote="\"", 
                              dec=".", sep='\t', 
                              colClasses='character', 
                              fill = TRUE,nrow=1)
      classColumns <- setNames(rep('character',ncol(namesData)),names(namesData))
      classColumns["ActivityStartDate"] <- "Date"
      
      classColumns[grep("MeasureValue",names(classColumns))] <- NA
      
      retval <- read.delim(textConnection(doc), header = TRUE, quote="\"", 
                           dec=".", sep='\t', 
                           colClasses=as.character(classColumns), 
                           fill = TRUE)    
      actualNumReturned <- nrow(retval)
      
      if(actualNumReturned != numToBeReturned) warning(numToBeReturned, " sample results were expected, ", actualNumReturned, " were returned")
      
      timeZoneLibrary <- setNames(c("America/New_York","America/New_York","America/Chicago","America/Chicago",
                                    "America/Denver","America/Denver","America/Los_Angeles","America/Los_Angeles",
                                    "America/Anchorage","America/Anchorage","America/Honolulu","America/Honolulu"),
                                  c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
      timeZoneStart <- as.character(timeZoneLibrary[retval$ActivityStartTime.TimeZoneCode])
      timeZoneEnd <- as.character(timeZoneLibrary[retval$ActivityEndTime.TimeZoneCode])
      
      if(any(!is.na(timeZoneStart))){
        if(length(unique(timeZoneStart)) == 1){
          retval$ActivityStartDateTime <- with(retval, as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time),format="%Y-%m-%d %H:%M:%S", tz=unique(timeZoneStart)))
        } else {
          warning("Mixed time zone information")
          if(any(is.na(timeZoneStart))){
            warning("Missing time zone information, all dateTimes default to user's local time")
            retval$ActivityStartDateTime <- with(retval, as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time), format="%Y-%m-%d %H:%M:%S"),tz=Sys.timezone())
          } else {
            for(i in seq_along(row.names(retval))){
              timeZone <- timeZoneStart[i]
              retval$ActivityStartDateTime[i] <- with(retval, as.POSIXct(paste(ActivityStartDate[i], ActivityStartTime.Time[i]), format="%Y-%m-%d %H:%M:%S",tz=timeZone))
            }
          }
        }
      }
      
      if(any(!is.na(timeZoneEnd))){
        if(length(unique(timeZoneEnd)) == 1){
          retval$ActivityEndDateTime <- with(retval, as.POSIXct(paste(ActivityEndDate, ActivityEndTime.Time), format="%Y-%m-%d %H:%M:%S",tz=unique(timeZoneEnd)))
        } else {
          warning("Mixed time zone information")
          if(any(is.na(timeZoneEnd))){
            warning("Missing time zone information, all dateTimes default to user's local time")
            retval$ActivityEndDateTime <- with(retval, as.POSIXct(paste(ActivityEndDate, ActivityEndTime.Time), format="%Y-%m-%d %H:%M:%S"), tz=Sys.timezone())
          } else {
            for(i in seq_along(row.names(retval))){
              retval$ActivityEndDateTime[i] <- with(retval, as.POSIXct(paste(ActivityEndDate[i], ActivityEndTime.Time[i]), format="%Y-%m-%d %H:%M:%S",tz=timeZoneEnd[i]))
            }
          }
        }
      }
      
      if(any(retval$ActivityEndDate != "")){
        retval$ActivityEndDate <- as.Date(retval$ActivityEndDate)
      }
      
      return(retval)
      
    } else {
      warning("No data to retrieve")
      return(NA)
    }
  } else {
    message(paste("URL caused an error:", url))
    message("Content-Type=",h$value()["Content-Type"])
    return(NA)
  }
}