#' add a water year column
#' 
#' Add a column to the dataRetrieval data frame with the water year. WQP 
#' queries will return a water year column for the start and end dates
#' of the data.
#' 
#' @param rawData the daily- or unit-values datset retrieved from NWISweb. 
#' @param dateColName character value specifying which column in the dataset 
#' should be used to calculate the water year. Defaults allow `dateTime`,
#' `Date`, `ActivityStartDateTime`, `ActivityEndDateTime`.
#' 
#' @return data.frame with an additional integer column called `waterYear`. For WQP,
#' there will be 2 columns - `startWaterYear` and `endWaterYear`.
#' @export
#' 
#' @importFrom dplyr select
#' @importFrom dplyr everything
#' @importFrom dplyr mutate_
#' @examples
#' \dontrun{ 
#' dataTemp <- readNWISdata(stateCd="OH",parameterCd="00010", service="dv")
#' dataTemp <- addWaterYear(dataTemp)
#' 
#' pHData <- readWQPdata(siteid="USGS-04024315",characteristicName="pH")
#' pHData <- addWaterYear(pHData)
#' }
addWaterYear <- function(rawData, dateColName = c("dateTime", "Date", "ActivityStartDateTime",
                                                  "ActivityEndDateTime")){

  # only allow WY to be added if there is an appropriate date column
  if(all(!dateColName %in% names(rawData))){
    stop("specified date column does not exist in supplied data frame")
  }
  
  # set the name of the date column to use for calculating the WY
  dateCol <- names(rawData)[names(rawData) %in% dateColName]
  
  # if both these columns are in the df, we need to have 2 WY cols
  isWQP <- all(c("ActivityStartDateTime", "ActivityEndDateTime") %in% dateCol)
  
  # if waterYear column already exists, don't add another
  if("waterYear" %in% names(rawData)){
    message("waterYear column already exists, returning df unchanged")
    return(rawData)
  }
  
  # add water year vector as new column(s)
  wyData <- rawData
  if(isWQP){

    startCol <- dateCol[grepl("start", tolower(dateCol))]
    endCol <- dateCol[grepl("end", tolower(dateCol))]
    wyData[["startWaterYear"]] <- calcWaterYear(wyData[[startCol]])
    wyData[["endWaterYear"]] <- calcWaterYear(wyData[[endCol]])
    
    # move the WY cols so that they is always come right after the date col
    startDateTime_i <- which(names(wyData) == startCol)
    endDateTime_i <- which(names(wyData) == endCol)
    wyData <- select(wyData, 1:endDateTime_i, startWaterYear, 
                     endWaterYear, everything())
    
  } else {
    wyData[["waterYear"]] <- calcWaterYear(wyData[[dateCol]])
    
    # move waterYear so that it is always comes right after dateTime
    dateTime_i <- which(names(wyData) == dateCol)
    wyData <- select(wyData, 1:dateTime_i, waterYear, everything())
  }

  return(wyData)
}

#' extract WY from a date
#' 
#' @param dateVec vector of dates as character ("YYYY-DD-MM"),
#' numeric, Date, or POSIXct
#' 
#' @keywords internal
calcWaterYear <- function(dateVec){
  # POSIXlt years start at 100, POSIXlt months start at 0
  dateTimeVec <- as.POSIXlt(dateVec)
  calYear <- dateTimeVec$year + 1900
  calMon <- dateTimeVec$mon + 1
  
  # when the date is NA, it should not try to add 1
  whichPastOct <- calMon >= 10
  whichPastOct[is.na(whichPastOct)] <- FALSE
  
  # add one to the year if it is in October or after
  waterYear <- calYear
  waterYear[whichPastOct] <- calYear[whichPastOct] + 1
  
  return(waterYear)
}
