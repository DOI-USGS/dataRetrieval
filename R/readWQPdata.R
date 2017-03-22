#' General Data Import from Water Quality Portal
#'
#' Imports data from Water Quality Portal web service. This function gets the data from here: \url{https://www.waterqualitydata.us}.
#' because it allows for other agencies rather than the USGS.  
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options. A list of arguments can also be supplied. 
#' @param querySummary logical to ONLY return the number of records and unique sites that will be returned from this query. This argument is not supported via the combined list from the \dots argument
#' @param tz timezone as a character string. See \code{OlsonNames()} for a list of possibilities.
#' @keywords data import WQP web service
#' @return A data frame with at least the following columns:
#' \tabular{lll}{ 
#' Name \tab Type \tab Description \cr
#' OrganizationIdentifier \tab character \tab  A designator used to uniquely identify a unique business establishment within a context.\cr
#' OrganizationFormalName \tab character \tab  The legal designator (i.e. formal name) of an organization.\cr
#' ActivityIdentifier \tab character \tab	Designator that uniquely identifies an activity within an organization.\cr
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
#' @examples
#' \dontrun{
#' nameToUse <- "pH"
#' pHData <- readWQPdata(siteid="USGS-04024315",characteristicName=nameToUse)
#' pHData_summary <- readWQPdata(bBox=c(-90.10,42.67,-88.64,43.35),
#'      characteristicName=nameToUse, querySummary=TRUE)
#' startDate <- as.Date("2013-01-01")
#' nutrientDaneCounty <- readWQPdata(countycode="US:55:025",startDate=startDate,
#'                         characteristicType="Nutrient")
#' secchi.names = c("Depth, Secchi disk depth", 
#'                  "Depth, Secchi disk depth (choice list)", 
#'                  "Secchi Reading Condition (choice list)", 
#'                  "Secchi depth", 
#'                  "Water transparency, Secchi disc")
#' args <- list('startDateLo' = startDate, 
#'              'startDateHi' = "2013-12-31", 
#'               statecode="WI", 
#'               characteristicName=secchi.names)
#' 
#' wqp.data <- readWQPdata(args)   
#' 
#' args_2 <- list('startDateLo' = startDate, 
#'              'startDateHi' = "2013-12-31", 
#'               statecode="WI", 
#'               characteristicName=secchi.names,
#'               querySummary=TRUE)
#'
#' wqp.summary <- readWQPdata(args_2) 
#' 
#' arg_3 <- list('startDateLo' = startDate, 
#'              'startDateHi' = "2013-12-31")
#' arg_4 <- list(statecode="WI", 
#'               characteristicName=secchi.names)
#' wqp.summary <- readWQPdata(arg_3, arg_4, querySummary=TRUE)
#' wqp.summary_WI <- readWQPdata(arg_3, statecode="WI", 
#'                               characteristicName=secchi.names, 
#'                               querySummary=TRUE)
#'                               
#' # querying by county
#' dailyLexingtonVA <- readWQPdata(statecode = "Virginia", 
#'                                 countycode="Lexington", 
#'                                 parameterCd = "00010") 
#'                                 
#' # Biological data:
#' bioData <- readWQPdata(statecode = "WI",
#'                        countycode = "Dane",
#'                        providers = "BIODATA")
#'                                 
#' }
readWQPdata <- function(..., querySummary=FALSE, tz="UTC"){
  
  tz <- match.arg(tz, OlsonNames())
  
  values <- readWQPdots(...)
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))

  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  baseURL <- drURL("wqpData")
  urlCall <- paste0(baseURL,
                   urlCall,
                   "&sorted=no&mimeType=tsv")

  if(querySummary){
    retquery <- getQuerySummary(urlCall)
    return(retquery)
  } else {
  
    retval <- importWQP(urlCall,zip=values["zip"] == "yes", tz=tz)
    
    if(!all(is.na(retval))){
      siteInfo <- whatWQPsites(...)
      
      siteInfoCommon <- data.frame(station_nm=siteInfo$MonitoringLocationName,
                                   agency_cd=siteInfo$OrganizationIdentifier,
                                   site_no=siteInfo$MonitoringLocationIdentifier,
                                   dec_lat_va=siteInfo$LatitudeMeasure,
                                   dec_lon_va=siteInfo$LongitudeMeasure,
                                   hucCd=siteInfo$HUCEightDigitCode,
                                   stringsAsFactors=FALSE)
      
      siteInfo <- cbind(siteInfoCommon, siteInfo)
      
      retvalVariableInfo <- retval[,c("CharacteristicName","USGSPCode",
                                      "ResultMeasure.MeasureUnitCode","ResultSampleFractionText")]
      retvalVariableInfo <- unique(retvalVariableInfo)
      
      variableInfo <- data.frame(characteristicName=retval$CharacteristicName,
                                 parameterCd=retval$USGSPCode,
                                 param_units=retval$ResultMeasure.MeasureUnitCode,
                                 valueType=retval$ResultSampleFractionText,
                                 stringsAsFactors=FALSE)
      
      if(any(!is.na(variableInfo$parameterCd))){
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
      attr(retval, "url") <- urlCall
      attr(retval, "queryTime") <- Sys.time()
      
      return(retval)
    } else {
      message("The following url returned no data:\n")
      message(urlCall)
    }
  }
}