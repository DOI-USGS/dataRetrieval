#' Populate Date Columns
#'
#' Creates various date columns for WRTDS study.
#'
#' @param rawData vector with dateTime
#' @return DateFrame dataframe
#' @export
#' @examples
#' dateTime <- c('1984-01-01', '1985-01-02', '1986-01-03')
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
  
  leapOffset <- ifelse((year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0)), 0,1)
  
  DateFrame$Day[DateFrame$Day > 59] <- DateFrame$Day[DateFrame$Day > 59] + leapOffset 
  
  DateFrame$DecYear <- year + (DateFrame$Day -0.5)/366
  DateFrame$MonthSeq <- ((year-1850)*12)+DateFrame$Month
  return (DateFrame)
  
}
