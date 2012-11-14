#' Populate Date Columns
#'
#' Creates various date columns for WRTDS study.
#'
#' @param rawData vector with dateTime
#' @return DateFrame dataframe
#' @export
#' @examples
#' dateTime <- c('1985-01-01', '1985-01-02', '1985-01-03')
#' populateDateColumns(dateTime)
populateDateColumns <- function(rawData){  # rawData is a vector of dates
  DateFrame <- as.data.frame(matrix(ncol=1,nrow=length(rawData)))
  colnames(DateFrame) <- c('Date')  
  DateFrame$Date <- rawData
  dateTime <- as.POSIXlt(rawData)
  DateFrame$Julian <- as.numeric(julian(dateTime,origin=as.Date("1850-01-01")))
  DateFrame$Month <- dateTime$mon + 1
  DateFrame$Day <- dateTime$yday + 1
  year <- dateTime$year + 1900
  DateFrame$DecYear <- year + (DateFrame$Day -0.5)/366
  DateFrame$MonthSeq <- ((year-1850)*12)+DateFrame$Month
  return (DateFrame)
  
}