#' Raw Data Import for Water Quality Portal
#'
#' Imports data from the Water Quality Portal. 
#' This function gets the data from here: \url{https://www.waterqualitydata.us}. There
#' are four required input arguments: siteNumbers, parameterCd, startDate, and endDate.
#' parameterCd can either be a USGS 5-digit code, or a characteristic name. The sites can be 
#' either USGS, or other Water Quality Portal offered sites. It is required to use the 'full'
#' site name, such as 'USGS-01234567'. 
#'
#' @param siteNumbers character site number. This needs to include the full agency code prefix.
#' @param parameterCd vector of USGS 5-digit parameter code or characteristicNames. 
#' Leaving this blank will return all of the measured values during the specified time period.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record. Date arguments are always specified in local time.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record. Date arguments are always specified in local time.
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the 
#' date times to UTC, properly accounting for daylight savings times based on the data's provided tz_cd column.
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()} 
#' for more information on time zones.
#' @param querySummary logical to look at number of records and unique sites that will be returned from this query.
#' @keywords data import USGS web service
#' @return A data frame with at least the following columns:
#' \tabular{lll}{ 
#' Name \tab Type \tab Description \cr
#' OrganizationIdentifier \tab character \tab  A designator used to uniquely identify a unique business establishment within a context.\cr
#' OrganizationFormalName \tab character \tab  The legal designator (i.e. formal name) of an organization.\cr
#' ActivityIdentifier \tab character \tab  Designator that uniquely identifies an activity within an organization.\cr
#' ActivityTypeCode \tab character \tab	The text describing the type of activity.\cr
#' ActivityMediaName \tab character \tab	Name or code indicating the environmental medium where the sample was taken.\cr
#' ActivityMediaSubdivisionName \tab character \tab	Name or code indicating the environmental matrix as a subdivision of the sample media.\cr
#' ActivityStartDate \tab character \tab	The calendar date on which the field activity is started.\cr
#' ActivityStartTime/Time \tab character \tab	The time of day that is reported when the field activity began, based on a 24-hour timescale.\cr
#' ActivityStartTime/TimeZoneCode \tab character \tab	The time zone for which the time of day is reported. Any of the longitudinal divisions of the earth's surface in which a standard time is kept.\cr
#' ActivityEndDate \tab character \tab	The calendar date when the field activity is completed.\cr
#' ActivityEndTime/Time \tab character \tab	The time of day that is reported when the field activity ended, based on a 24-hour timescale.\cr
#' ActivityEndTime/TimeZoneCode \tab character \tab	The time zone for which the time of day is reported. Any of the longitudinal divisions of the earth's surface in which a standard time is kept.\cr
#' ActivityDepthHeightMeasure/MeasureValue \tab character \tab	A measurement of the vertical location (measured from a reference point) at which an activity occurred. Measure value is given in the units stored in ActivityDepthHeightMeasure/MeasureUnitCode.\cr
#' ActivityDepthHeightMeasure/MeasureUnitCode \tab character \tab	The code that represents the unit for measuring the item.\cr
#' ActivityDepthAltitudeReferencePointText \tab character \tab	The reference used to indicate the datum or reference used to establish the depth/altitude of an activity.\cr
#' ActivityTopDepthHeightMeasure/MeasureValue \tab character \tab	A measurement of the upper vertical location of a vertical location range (measured from a reference point) at which an activity occurred. Measure value is given in the units stored in ActivityTopDepthHeightMeasure/MeasureUnitCode.\cr
#' ActivityTopDepthHeightMeasure/MeasureUnitCode \tab character \tab	The code that represents the unit for measuring the item.\cr
#' ActivityBottomDepthHeightMeasure/MeasureValue \tab character \tab	A measurement of the lower vertical location of a vertical location range (measured from a reference point) at which an activity occurred. Measure value is given in the units stored in ActivityBottomDepthHeightMeasure/MeasureUnitCode.\cr
#' ActivityBottomDepthHeightMeasure/MeasureUnitCode \tab character \tab	The code that represents the unit for measuring the item.\cr
#' ProjectIdentifier \tab character \tab 	A designator used to uniquely identify a data collection project within a context of an organization.\cr
#' ActivityConductingOrganizationText \tab character \tab	A name of the Organization conducting an activity.\cr
#' MonitoringLocationIdentifier \tab character \tab	A designator used to describe the unique name, number, or code assigned to identify the monitoring location.\cr
#' ActivityCommentText \tab character \tab	General comments concerning the activity.\cr
#' SampleAquifer * \tab character \tab 	A code that designates the aquifer associated with groundwater samples.\cr
#' HydrologicCondition * \tab character \tab 	Hydrologic condition is the hydrologic condition that is represented by the sample collected (i.e. ? normal, falling, rising, peak stage).\cr
#' HydrologicEvent * \tab character \tab 	A hydrologic event that is represented by the sample collected (i.e. - storm, drought, snowmelt).\cr
#' SampleCollectionMethod/MethodIdentifier\tab character \tab 	The identification number or code assigned by the method publisher.\cr
#' SampleCollectionMethod/MethodIdentifierContext \tab character \tab	Identifies the source or data system that created or defined the identifier.\cr
#' SampleCollectionMethod/MethodName \tab character \tab	The title that appears on the method from the method publisher.\cr
#' SampleCollectionEquipmentName \tab character \tab	The name for the equipment used in collecting the sample.\cr
#' ResultDetectionConditionText \tab character \tab	The textual descriptor of a result.\cr
#' CharacteristicName \tab character \tab	The object, property, or substance which is evaluated or enumerated by either a direct field measurement, a direct field observation, or by laboratory analysis of material collected in the field.\cr
#' ResultSampleFractionText \tab character \tab	The text name of the portion of the sample associated with results obtained from a physically-partitioned sample.\cr
#' ResultMeasureValue \tab numeric \tab	The reportable measure of the result for the chemical, microbiological or other characteristic being analyzed. Measure value is given in the units stored in ResultMeasure/MeasureUnitCode.\cr
#' MeasureQualifierCode \tab character \tab	A code used to identify any qualifying issues that affect the results.\cr
#' ResultMeasure/MeasureUnitCode \tab character \tab	The code that represents the unit for measuring the item.\cr
#' ResultStatusIdentifier \tab character \tab	Indicates the acceptability of the result with respect to QA/QC criteria.\cr
#' StatisticalBaseCode \tab character \tab	The code for the method used to calculate derived results.\cr
#' ResultValueTypeName \tab character \tab	A name that qualifies the process which was used in the determination of the result value (e.g., actual, estimated, calculated).\cr
#' ResultWeightBasisText \tab character \tab	The name that represents the form of the sample or portion of the sample which is associated with the result value (e.g., wet weight, dry weight, ash-free dry weight).\cr
#' ResultTimeBasisText \tab character \tab	The period of time (in days) over which a measurement was made. For example, BOD can be measured as 5 day or 20 day BOD.\cr
#' ResultTemperatureBasisText \tab character \tab	The name that represents the controlled temperature at which the sample was maintained during analysis, e.g. 25 deg BOD analysis.\cr
#' ResultParticleSizeBasisText \tab character \tab	User defined free text describing the particle size class for which the associated result is defined.\cr
#' PrecisionValue \tab character \tab	A measure of mutual agreement among individual measurements of the same property usually under prescribed similar conditions.\cr
#' ResultCommentText \tab character \tab	Free text with general comments concerning the result.\cr
#' USGSPCode * \tab character \tab 	5-digit number used in the US Geological Survey computerized data system, National Water Information System (NWIS), to uniquely identify a specific constituent.\cr
#' ResultDepthHeightMeasure/MeasureValue + \tab character \tab 	A measurement of the vertical location (measured from a reference point) at which a result occurred.\cr
#' ResultDepthHeightMeasure/MeasureUnitCode + \tab character \tab	The code that represents the unit for measuring the item.\cr
#' ResultDepthAltitudeReferencePointText + \tab character \tab 	The reference used to indicate the datum or reference used to establish the depth/altitude of a result.\cr
#' SubjectTaxonomicName \tab character \tab	The name of the organism from which a tissue sample was taken.\cr
#' SampleTissueAnatomyName  * \tab character \tab 	The name of the anatomy from which a tissue sample was taken.\cr
#' ResultAnalyticalMethod/MethodIdentifier \tab character \tab	The identification number or code assigned by the method publisher.\cr
#' ResultAnalyticalMethod/MethodIdentifierContext \tab character \tab	Identifies the source or data system that created or defined the identifier.\cr
#' ResultAnalyticalMethod/MethodName \tab character \tab	The title that appears on the method from the method publisher.\cr
#' MethodDescriptionText * \tab character \tab 	A brief summary that provides general information about the method.\cr
#' LaboratoryName \tab character \tab	The name of Lab responsible for the result.\cr
#' AnalysisStartDate \tab character \tab	The calendar date on which the analysis began.\cr
#' ResultLaboratoryCommentText \tab character \tab	Remarks which further describe the laboratory procedures which produced the result.\cr
#' DetectionQuantitationLimitTypeName \tab character \tab	Text describing the type of detection or quantitation level used in the analysis of a characteristic.\cr
#' DetectionQuantitationLimitMeasure/MeasureValue \tab numeric \tab	Constituent concentration that, when processed through the complete method, produces a signal that is statistically different from a blank. Measure value is given in the units stored in DetectionQuantitationLimitMeasure/MeasureUnitCode.\cr
#' DetectionQuantitationLimitMeasure/MeasureUnitCode \tab character \tab	The code that represents the unit for measuring the item.\cr
#' PreparationStartDate \tab character \tab	The calendar date when the preparation/extraction of the sample for analysis began.\cr
#' ActivityStartDateTime \tab POSIXct \tab Activity start date and time converted to POSIXct UTC.\cr
#' ActivityEndDateTime \tab POSIXct \tab Activity end date and time converted to POSIXct UTC.\cr
#' }
#' * = elements only in NWIS
#' + = elements only in STORET
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' variableInfo \tab data.frame \tab A data frame containing information on the requested parameters \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' @export
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}}, 
#' \code{\link{readNWISqw}}, and \code{\link{importWQP}}
#' @examples
#' \dontrun{
#' rawPcode <- readWQPqw('USGS-01594440','01075', '', '')
#' rawCharacteristicName <- readWQPqw('WIDNR_WQX-10032762','Specific conductance', '', '')
#' rawPHsites <- readWQPqw(c('USGS-05406450', 'USGS-05427949','WIDNR_WQX-133040'), 'pH','','')
#' nwisEx <- readWQPqw('USGS-04024000',c('34247','30234','32104','34220'),'','2012-12-20')
#' nwisEx.summary <- readWQPqw('USGS-04024000',c('34247','30234','32104','34220'),
#'     '','2012-12-20', querySummary=TRUE)
#' }
readWQPqw <- function(siteNumbers,parameterCd,startDate="",endDate="",tz="UTC", querySummary=FALSE){

  url <- constructWQPURL(siteNumbers,parameterCd,startDate,endDate)
  
  if(querySummary){
    retquery <- getQuerySummary(url)
    return(retquery)
  } else {
    
    retval <- importWQP(url, tz = tz)
    
    pcodeCheck <- all(nchar(parameterCd) == 5) & all(!is.na(suppressWarnings(as.numeric(parameterCd))))
    
    if (nzchar(startDate)){
      startDate <- format(as.Date(startDate), format="%m-%d-%Y")
    }
    
    if (nzchar(endDate)){
      endDate <- format(as.Date(endDate), format="%m-%d-%Y")
    }
    
    if(pcodeCheck){
      siteInfo <- whatWQPsites(siteid=paste0(siteNumbers,collapse=";"),
                               pCode=paste0(parameterCd,collapse=";"), 
                               startDateLo=startDate, startDateHi=endDate)
    } else {
      siteInfo <- whatWQPsites(siteid=paste0(siteNumbers,collapse=";"), 
                               characteristicName=URLencode(paste0(parameterCd,collapse=";")), 
                               startDateLo=startDate, startDateHi=endDate)
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
    
    if(!anyNA(variableInfo$parameterCd)){
      pcodes <- unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)])
      pcodes <- pcodes["" != pcodes]
      paramINFO <- readNWISpCode(pcodes)
      names(paramINFO)["parameter_cd" == names(paramINFO)] <- "parameterCd"
      
      pCodeToName <- pCodeToName
      varExtras <- pCodeToName[pCodeToName$parm_cd %in% unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)]),]
      names(varExtras)[names(varExtras) == "parm_cd"] <- "parameterCd"
      variableInfo <- merge(variableInfo, varExtras, by="parameterCd", all = TRUE)
      variableInfo <- merge(variableInfo, paramINFO, by="parameterCd", all = TRUE)
      variableInfo <- unique(variableInfo)
    }
    
    attr(retval, "siteInfo") <- siteInfo
    attr(retval, "variableInfo") <- variableInfo
    attr(retval, "url") <- url
    attr(retval, "queryTime") <- Sys.time()
    
    return(retval)
  }
}
