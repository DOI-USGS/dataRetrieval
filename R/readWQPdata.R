#' General Data Import from Water Quality Portal
#'
#' Imports data from Water Quality Portal web service. This function gets the data from here: \url{http://www.waterqualitydata.us}.
#' because it allows for other agencies rather than the USGS.  
#'
#' @param \dots see \url{www.waterqualitydata.us/webservices_documentation.jsp} for a complete list of options
#' @keywords data import WQP web service
#' @return retval dataframe with first column dateTime, and at least one qualifier and value columns
#' (subsequent qualifier/value columns could follow depending on requested parameter codes)
#' @export
#' @examples
#' \dontrun{
#' nameToUse <- "pH"
#' pHData <- readWQPdata(siteid="USGS-04024315",characteristicName=nameToUse)
#' pHDataExpanded <- readWQPdata(bBox="-90.10,42.67,-88.64,43.35",characteristicName=nameToUse)
#' }
readWQPdata <- function(...){
  
  matchReturn <- list(...)
  
  options <- c("bBox","lat","long","within","countrycode","statecode","countycode","siteType","organization",
               "siteid","huc","sampleMedia","characteristicType","characteristicName","pCode","activityId",
               "startDateLo","startDateHi","mimeType","Zip","providers")
  
  if(!all(names(matchReturn) %in% options)) warning(matchReturn[!(names(matchReturn) %in% options)],"is not a valid query parameter to the Water Quality Portal")
  
  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse="",sep=""))))
  
  values <- gsub(",","%2C",values)
  values <- gsub("%20","+",values)
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- "http://www.waterqualitydata.us/Result/search?"
  urlCall <- paste0(baseURL,
                   urlCall,
                   "&mimeType=tsv")

  retval <- importWQP(urlCall,FALSE)
  
  siteInfo <- whatWQPsites(...)
  
  siteInfoCommon <- data.frame(station_nm=siteInfo$MonitoringLocationName,
                               agency_cd=siteInfo$OrganizationIdentifier,
                               site_no=siteInfo$MonitoringLocationIdentifier,
                               dec_lat_va=siteInfo$LatitudeMeasure,
                               dec_lon_va=siteInfo$LongitudeMeasure,
                               hucCd=siteInfo$HUCEightDigitCode,
                               stringsAsFactors=FALSE)
  
  siteInfo <- cbind(siteInfoCommon, siteInfo)
  
  
  variableInfo <- data.frame(characteristicName=retval$CharacteristicName,
                             parameterCd=retval$USGSPCode,
                             param_units=retval$ResultMeasure.MeasureUnitCode,
                             valueType=retval$ResultSampleFractionText,
                             stringsAsFactors=FALSE)
  variableInfo <- unique(variableInfo)
  
  if(any(!is.na(variableInfo$parameterCd))){
    pCodeToName <- pCodeToName
    varExtras <- pCodeToName[pCodeToName$parm_cd %in% unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)]),]
    names(varExtras)[names(varExtras) == "parm_cd"] <- "parameterCd"
    variableInfo <- merge(variableInfo, varExtras, by="parameterCd")
  }
  
  attr(retval, "siteInfo") <- siteInfo
  attr(retval, "variableInfo") <- variableInfo
  attr(retval, "url") <- urlCall
  attr(retval, "queryTime") <- Sys.time()
  
  return(retval)
  
}