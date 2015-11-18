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
#' @import data.table 
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
        
        if(headerInfo['status'] == "200"){
          doc <- unzip(temp)
          retval <- fread(doc,colClasses = "character",verbose = FALSE,showProgress = FALSE)
        } else {
          stop("Status:", headerInfo['status'], ": ", headerInfo['statusMessage'], "\nFor: ", obs_url)
        }
        unlink(doc)
      } else {
        retval <- suppressWarnings(fread(obs_url,colClasses = "character",verbose = FALSE,showProgress = FALSE))
      }
    } else {
      stop("Status:", headerInfo['status'], ": ", headerInfo['statusMessage'], "\nFor: ", obs_url)
    }
    
    actualNumReturned <- nrow(retval)
    if(actualNumReturned != numToBeReturned) warning(numToBeReturned, " sample results were expected, ", actualNumReturned, " were returned")
    
  } else {
    
    if(zip){
      doc <- unzip(obs_url)
      retval <- fread(doc,colClasses = "character",verbose = FALSE,showProgress = FALSE)
      unlink(doc)
    }  else {
      retval <- fread(obs_url,colClasses = "character",verbose = FALSE,showProgress = FALSE)
    }

  }

  dateCols <- c("ActivityStartDate","ActivityEndDate","AnalysisStartDate","PreparationStartDate")
  retval <- suppressWarnings(retval[, (dateCols) := lapply(.SD, function(x) as.Date(parse_date_time(x, c("Ymd", "mdY")))),
                                      .SDcols = dateCols])
  
  numTmp <- names(retval)[grep("Value",names(retval))]
  retval <- suppressWarnings(retval[, (numTmp) := lapply(.SD, as.numeric), .SDcols = numTmp])
  
  offsetLibrary <- data.table(offset=c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10),
                              code=c("EST","EDT","CST","CDT","MST","MDT","PST","PDT","AKST","AKDT","HAST","HST"),
                              key = "code")
  retval <- setkey(retval, ActivityStartTime/TimeZoneCode)
  retval <- retval[,timeZoneStart:=offsetLibrary[SJ(retval$`ActivityStartTime/TimeZoneCode`)]$offset]
  retval <- setkey(retval, ActivityEndTime/TimeZoneCode)
  retval <- retval[,timeZoneEnd:=offsetLibrary[SJ(retval$`ActivityEndTime/TimeZoneCode`)]$offset]
  
  retval <- retval[,ActivityStartDateTime:=paste(ActivityStartDate, `ActivityStartTime/Time`)]
  retval <- retval[,ActivityStartDateTime:=fast_strptime(ActivityStartDateTime, '%Y-%m-%d %H:%M:%S')+60*60*timeZoneStart]
  
  retval <- retval[,ActivityEndDateTime:=paste(ActivityEndDate, `ActivityEndTime/Time`)]
  retval <- retval[,ActivityEndDateTime:=fast_strptime(ActivityEndDateTime, '%Y-%m-%d %H:%M:%S')+60*60*timeZoneEnd]
  


#   if(all(is.na(retval$ActivityEndDateTime))){
#     retval$ActivityEndDateTime <- NULL
#   }

  retval <- retval[order(OrganizationIdentifier, 
                         MonitoringLocationIdentifier, 
                         ActivityStartDateTime)]
  
  retval <- setDF(retval)
  names(retval) <- gsub("/",".",names(retval))
  return(retval)
  
  

}