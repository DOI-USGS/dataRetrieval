#' add a water year column
#' 
#' add a column to the dataRetrieval data frame with the water year
#' 
#' @param rawData the daily- or unit-values datset retrieved from NWISweb. 
#' 
#' @return data.frame with an additional integer column called waterYear
#' @export
#' @examples
#' \dontrun{ 
#' dataTemp <- readNWISdata(stateCd="OH",parameterCd="00010", service="dv")
#' dataTemp <- addWaterYear(dataTemp)
#' }
addWaterYear <- function(rawData, ...){

  # figure out water year
  # POSIXlt years start at 100, POSIXlt months start at 0
  dateTimeVec <- as.POSIXlt(rawData[['dateTime']])
  calYear <- dateTimeVec$year + 1900
  calMon <- dateTimeVec$mon + 1
  waterYear <- calYear
  waterYear[calMon >= 10] <- calYear[calMon >= 10] + 1 
  
  # add water year vector as new column
  rawData$waterYear <- waterYear
  
  return(rawData)
}
