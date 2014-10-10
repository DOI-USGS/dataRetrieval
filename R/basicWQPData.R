#' Basic Water Quality Portal Data grabber
#'
#' Imports data from the Water Quality Portal based on a specified url.
#' 
#' @param url string URL to Water Quality Portal#' @keywords data import USGS web service
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally, a POSIXct dateTime column is supplied for 
#' start and end times.
#' @export
#' @import RCurl
#' @importFrom lubridate parse_date_time
#' @examples
#' # These examples require an internet connection to run
#' \dontrun{
#' ## Examples take longer than 5 seconds:
#' rawSampleURL <- constructNWISURL('USGS-01594440','01075', '1985-01-01', '1985-03-31',"wqp")
#' rawSample <- readWQPData(rawSampleURL)
#' }
readWQPData <- function(url){
  
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
      
      classColumns[grep("MeasureValue",names(classColumns))] <- NA
      
      retval <- read.delim(textConnection(doc), header = TRUE, quote="\"", 
                           dec=".", sep='\t', 
                           colClasses=as.character(classColumns), 
                           fill = TRUE)    
      actualNumReturned <- nrow(retval)
      
      retval[,names(which(sapply(retval[,grep("MeasureValue",names(retval))], function(x)all(is.na(x)))))] <- ""
      
      if(actualNumReturned != numToBeReturned) warning(numToBeReturned, " sample results were expected, ", actualNumReturned, " were returned")
      
      timeZoneLibrary <- setNames(c("America/New_York","America/New_York","America/Chicago","America/Chicago",
                                    "America/Denver","America/Denver","America/Los_Angeles","America/Los_Angeles",
                                    "America/Anchorage","America/Anchorage","America/Honolulu","America/Honolulu"),
                                  c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"))
      timeZoneStart <- as.character(timeZoneLibrary[retval$ActivityStartTime.TimeZoneCode])
      timeZoneEnd <- as.character(timeZoneLibrary[retval$ActivityEndTime.TimeZoneCode])
      timeZoneStart[is.na(timeZoneStart)] <- ""
      timeZoneEnd[is.na(timeZoneEnd)] <- ""
      
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
        if(length(unique(timeZoneStart)) == 1){
          retval$ActivityStartDateTime <- with(retval, as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time),format="%Y-%m-%d %H:%M:%S", tz=unique(timeZoneStart)))
        } else {
          
          mostCommonTZ <- names(sort(summary(as.factor(timeZoneStart)),decreasing = TRUE)[1])

          retval$ActivityStartDateTime <- with(retval, as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time),
                                format="%Y-%m-%d %H:%M:%S", 
                                tz=mostCommonTZ))
          additionalTZs <- names(sort(summary(as.factor(timeZoneStart)),decreasing = TRUE)[-1])
          for(i in additionalTZs){
            retval$ActivityStartDateTime[timeZoneStart == i] <-  with(retval[timeZoneStart == i,], 
                               as.POSIXct(paste(ActivityStartDate, ActivityStartTime.Time),
                               format="%Y-%m-%d %H:%M:%S", 
                               tz=i))      
          }
        }
      }
      
      if(any(!is.na(timeZoneEnd))){
        if(length(unique(timeZoneEnd)) == 1){
          retval$ActivityEndDateTime <- with(retval, as.POSIXct(paste(ActivityEndDate, ActivityEndTime.Time), format="%Y-%m-%d %H:%M:%S",tz=unique(timeZoneEnd)))
        } else {
          mostCommonTZ <- names(sort(summary(as.factor(timeZoneEnd)),decreasing = TRUE)[1])
          
          retval$ActivityEndDateTime <- with(retval, as.POSIXct(paste(ActivityEndDate, ActivityEndTime.Time),
                                      format="%Y-%m-%d %H:%M:%S", 
                                      tz=mostCommonTZ))
          additionalTZs <- names(sort(summary(as.factor(timeZoneEnd)),decreasing = TRUE)[-1])
          for(i in additionalTZs){
            retval$ActivityEndDateTime[timeZoneEnd == i] <-  with(retval[timeZoneEnd == i,], 
                          as.POSIXct(paste(ActivityEndDate, ActivityEndTime.Time),
                                     format="%Y-%m-%d %H:%M:%S", 
                                     tz=i))      
          }
        }
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