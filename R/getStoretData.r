#' Storet Data Import for Water Quality Data
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://www.waterqualitydata.us}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param ParameterCd string USGS parameter code.  This is usually an 5 digit number. Multiple parameter codes can be inputted with a ';' separator.  Leaving this blank will return all of the measured values during the specified time period.
#' @param StartDate string starting date for data retrieval in the form YYYY-MM-DD.
#' @param EndDate string ending date for data retrieval in the form YYYY-MM-DD.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service
#' @return retval dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on requested parameter codes)
#' @export
#' @examples
#' # These examples require an internet connection to run
#' \dontrun{getStoretData('01594440','01075', '1985-01-01', '1985-03-31')}
#' \dontrun{getStoretData('05114000','', '1985-01-01', '1985-03-31')}
#' \dontrun{getStoretData('05114000','00915;00931', '1985-01-01', '1985-04-30', interactive=FALSE)}
getStoretData <- function(siteNumber,characteristicName,StartDate,EndDate,interactive=TRUE){
  siteNumber <- formatCheckSiteNumber(siteNumber, interactive=interactive)
  StartDate <- formatCheckDate(StartDate, "StartDate", interactive=interactive)
  EndDate <- formatCheckDate(EndDate, "EndDate", interactive=interactive)
  
  dateReturn <- checkStartEndDate(StartDate, EndDate, interactive=interactive)
  StartDate <- dateReturn[1]
  EndDate <- dateReturn[2]
  
  if (nzchar(StartDate)){
    StartDate <- format(as.Date(StartDate), format="%m-%d-%Y")
  }
  if (nzchar(EndDate)){
    EndDate <- format(as.Date(EndDate), format="%m-%d-%Y")
  }
  
  baseURL <- "http://www.waterqualitydata.us/Result/search?siteid="
  url <- paste(baseURL,
               siteNumber,
               "&characteristicName=",
               characteristicName,   # to get multi-parameters, use a semicolen 
               "&startDateLo=",
               StartDate,
               "&startDateHi=",
               EndDate,
               "&countrycode=US&mimeType=tsv",sep = "")
  
  suppressWarnings(retval <- read.delim(url, header = TRUE, quote="\"", dec=".", sep='\t', colClasses=c('character'), fill = TRUE))
  
  qualifier <- ifelse((
    (
      retval$ResultDetectionConditionText == "Not Detected"
      & length(grep("Lower", retval$DetectionQuantitationLimitTypeName)) > 0
    )
    | 
      (
        retval$ResultMeasureValue < retval$DetectionQuantitationLimitMeasure.MeasureValue
        & retval$ResultValueTypeName == "Actual"
      )
  ),
                      "<",
                      ""
  )
  
  correctedData<-ifelse((nchar(qualifier)==0),retval$ResultMeasureValue,retval$DetectionQuantitationLimitMeasure.MeasureValue)
  test <- data.frame(retval$USGSPCode)
  
  #   test$dateTime <- as.POSIXct(strptime(paste(retval$ActivityStartDate,retval$ActivityStartTime.Time,sep=" "), "%Y-%m-%d %H:%M:%S"))
  test$dateTime <- as.Date(retval$ActivityStartDate, "%Y-%m-%d")
  
  originalLength <- nrow(test)
  test$qualifier <- qualifier
  test$value <- as.numeric(correctedData)
  
  test <- test[!is.na(test$dateTime),]
  newLength <- nrow(test)
  if (originalLength != newLength){
    numberRemoved <- originalLength - newLength
    warningMessage <- paste(numberRemoved, " rows removed because no date was specified", sep="")
    warning(warningMessage)
  }
  
  colnames(test)<- c("USGSPCode","dateTime","qualifier","value")
  data <- reshape(test, idvar="dateTime", timevar = "USGSPCode", direction="wide")    
  data$dateTime <- format(data$dateTime, "%Y-%m-%d")
  data$dateTime <- as.Date(data$dateTime)
  return(data)
}