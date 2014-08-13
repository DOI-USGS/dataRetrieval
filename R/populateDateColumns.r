#' Populate Date Columns
#'
#' Creates various date columns for WRTDS study.
#'
#' @param rawData vector with dateTime
#' @return DateFrame dataframe
#' @export
#' @examples
#' dateTime <- c('1984-02-28 13:56', '1984-03-01', '1986-03-01')
#' expandedDateDF <- populateDateColumns(dateTime)
populateDateColumns <- function(rawData){  # rawData is a vector of dates
  DateFrame <- as.data.frame(matrix(ncol=1,nrow=length(rawData)))
  colnames(DateFrame) <- c('Date')  
  DateFrame$Date <- rawData
  dateTime <- as.POSIXlt(rawData)
  DateFrame$Julian <- as.numeric(julian(dateTime,origin=as.Date("1850-01-01")))
  DateFrame$Month <- dateTime$mon + 1
  DateFrame$Day <- dateTime$yday + 1
  year <- dateTime$year + 1900
  hour <- dateTime$hour
  minute <- dateTime$min
  
  if (sum(hour) == 0 & sum(minute) == 0){
    dateTime$hour <- rep(12,length(dateTime))
  }
  
  leapOffset <- ifelse((year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0)), 0,1)
  
  DateFrame$Day[DateFrame$Day > 59] <- DateFrame$Day[DateFrame$Day > 59] + leapOffset[DateFrame$Day > 59]
  
#   DateFrame$DecYear <- year + (DateFrame$Day -0.5)/366

  jan1 <- as.POSIXlt(paste(year,"-01-01",sep=""),format="%Y-%m-%d")
  jan1NextYear <- as.POSIXlt(paste(year+1,"-01-01",sep=""),format="%Y-%m-%d")
  decimal <- as.numeric(difftime(dateTime, jan1, units = "secs"))
  decimal <- decimal/as.numeric(difftime(jan1NextYear, jan1, units = "secs"))
  
  DateFrame$DecYear <- year + decimal
  DateFrame$MonthSeq <- ((year-1850)*12)+DateFrame$Month
  return (DateFrame)
  
}
