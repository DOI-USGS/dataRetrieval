#' add a water year column
#' 
#' add a column to the dataRetrieval data frame with the water year
#' 
#' @param rawData the daily- or unit-values datset retrieved from NWISweb. 
#' 
#' @return data.frame with an additional integer column called waterYear
#' @export
#' 
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @examples
#' \dontrun{ 
#' dataTemp <- readNWISdata(stateCd="OH",parameterCd="00010", service="dv")
#' dataTemp <- addWaterYear(dataTemp)
#' }
addWaterYear <- function(rawData, dateColName = c("dateTime", "Date")){

  # only allow WY to be added if there is an appropriate date column
  if(all(!dateColName %in% names(rawData))){
    stop("specified date column does not exist in supplied data frame")
  }
  
  # set the name of the date column to use for calculating the WY
  dateCol <- names(rawData)[names(rawData) %in% dateColName]
  
  # if waterYear column already exists, don't add another
  if("waterYear" %in% names(rawData)){
    message("waterYear column already exists, returning df unchanged")
    return(rawData)
  }
  
  # figure out water year
  # POSIXlt years start at 100, POSIXlt months start at 0
  dateTimeVec <- as.POSIXlt(rawData[[dateCol]])
  calYear <- dateTimeVec$year + 1900
  calMon <- dateTimeVec$mon + 1
  waterYear <- calYear
  waterYear[calMon >= 10] <- calYear[calMon >= 10] + 1 
  
  # add water year vector as new column
  wyData <- rawData
  wyData$waterYear <- waterYear
  
  # move waterYear so that it is always comes right after dateTime
  dateTime_i <- which(names(wyData) == dateCol)
  wyData <- select(wyData, 1:dateTime_i, waterYear, everything())
  
  return(wyData)
}
