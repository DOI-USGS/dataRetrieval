#' Raw Data Import for Water Quality Portal
#'
#' Imports data from the Water Quality Portal. 
#' This function gets the data from here: \url{http://www.waterqualitydata.us}. There
#' are four required input arguments: siteNumber, parameterCd, startDate, and endDate.
#' parameterCd can either be a USGS 5-digit code, or a characteristic name. The sites can be 
#' either USGS, or other Water Quality Portal offered sites. It is required to use the 'full'
#' site name, such as 'USGS-01234567'. 
#'
#' @param siteNumber character site number. This needs to include the full agency code prefix.
#' @param parameterCd vector of USGS 5-digit parameter code or characteristicNames. 
#' Leaving this blank will return all of the measured values during the specified time period.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @keywords data import USGS web service
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally, a POSIXct dateTime column is supplied for 
#' start and end times.
#' @export
#' @import RCurl
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}}, 
#' \code{\link{readNWISqw}}, and \code{\link{importWQP}}
#' @examples
#' \dontrun{
#' rawPcode <- readWQPqw('USGS-01594440','01075', '', '')
#' rawCharacteristicName <- readWQPqw('WIDNR_WQX-10032762','Specific conductance', '', '')
#' rawPHmultiSite <- readWQPqw(c('USGS-05406450', 'USGS-05427949','WIDNR_WQX-133040'), 'pH','','')
#' }
readWQPqw <- function(siteNumbers,parameterCd,startDate="",endDate=""){

  url <- constructWQPURL(siteNumbers,parameterCd,startDate,endDate)
  retval <- importWQP(url)
  
  pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
  
  if (nzchar(startDate)){
    startDate <- format(as.Date(startDate), format="%m-%d-%Y")
  }
  
  if (nzchar(endDate)){
    endDate <- format(as.Date(endDate), format="%m-%d-%Y")
  }
  
  if(pcodeCheck){
    siteInfo <- whatWQPsites(siteid=paste0(siteNumbers,";"),
                             pCode=parameterCd, startDateLo=startDate, startDateHi=endDate)
  } else {
    siteInfo <- whatWQPsites(siteid=paste0(siteNumbers,";"), 
                             characteristicName=parameterCd, startDateLo=startDate, startDateHi=endDate)
  }
    
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
  
  if(any(variableInfo$parameterCd != "")){
    pCodeToName <- pCodeToName
    varExtras <- pCodeToName[pCodeToName$parm_cd %in% unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)]),]
    names(varExtras)[names(varExtras) == "parm_cd"] <- "parameterCd"
    variableInfo <- merge(variableInfo, varExtras, by="parameterCd")
  }
  
  attr(retval, "siteInfo") <- siteInfo
  attr(retval, "variableInfo") <- variableInfo
  attr(retval, "url") <- url
  attr(retval, "queryTime") <- Sys.time()
  
  return(retval)
  
}
