#' Site Data Import from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on \url{https://www.waterqualitydata.us/webservices_documentation}. The return from
#' this function returns the basic metadata on WQP sites. It is
#' generally faster than the \code{\link{whatWQPdata}} function, but does
#' not return information on what data was collected at the site.
#'
#' The \code{readWQPsummary} function has
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options. A list of arguments can also be supplied.
#' @keywords data import WQP web service
#' @rdname wqpSpecials
#' @name whatWQPsites
#' @return A data frame with at least the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' OrganizationIdentifier \tab character \tab  A designator used to uniquely identify a unique business establishment within a context. \cr
#' OrganizationFormalName \tab character \tab	The legal designator (i.e. formal name) of an organization. \cr
#' MonitoringLocationIdentifier \tab character \tab	A designator used to describe the unique name, number, or code assigned to identify the monitoring location. \cr
#' MonitoringLocationName \tab character \tab	The designator specified by the sampling organization for the site at which sampling or other activities are conducted. \cr
#' MonitoringLocationTypeName \tab character \tab	The descriptive name for a type of monitoring location. \cr
#' MonitoringLocationDescriptionText \tab character \tab 	Text description of the monitoring location. \cr
#' HUCEightDigitCode \tab character \tab	The 8 digit federal code used to identify the hydrologic unit of the monitoring location to the cataloging unit level of precision. \cr
#' DrainageAreaMeasure/MeasureValue * \tab character \tab	The drainage basin of a lake, stream, wetland, or estuary site. Measure value is given in the units stored in DrainageAreaMeasure/MeasureUnitCode. \cr
#' DrainageAreaMeasure/MeasureUnitCode * \tab character \tab	The code that represents the unit for measuring the item. \cr
#' ContributingDrainageAreaMeasure/MeasureValue * \tab character \tab	The contributing drainage area of a lake, stream, wetland, or estuary site. Measure value is given in the units stored in ContributingDrainageAreaMeasure/MeasureUnitCode. \cr
#' ContributingDrainageAreaMeasure/MeasureUnitCode * \tab character \tab	The code that represents the unit for measuring the item. \cr
#' LatitudeMeasure \tab numeric \tab	The measure of the angular distance on a meridian north or south of the equator. \cr
#' LongitudeMeasure \tab numeric \tab	The measure of the angular distance on a meridian east or west of the prime meridian. \cr
#' SourceMapScaleNumeric \tab character \tab	The number that represents the proportional distance on the ground for one unit of measure on the map or photo. \cr
#' HorizontalAccuracyMeasure/MeasureValue * \tab character \tab	The horizontal measure of the relative accuracy of the latitude and longitude coordinates." Measure value is given in the units stored in HorizontalAccuracyMeasure/MeasureUnitCode. \cr
#' HorizontalAccuracyMeasure/MeasureUnitCode * \tab character \tab	The code that represents the unit for measuring the item. \cr
#' HorizontalCollectionMethodName \tab character \tab	The name that identifies the method used to determine the latitude and longitude coordinates for a point on the earth. \cr
#' HorizontalCoordinateReferenceSystemDatumName \tab character \tab	The name that describes the reference datum used in determining latitude and longitude coordinates. \cr
#' VerticalMeasure/MeasureValue \tab character \tab	The measure of elevation (i.e., the altitude), above or below a reference datum. Measure value is given in the units stored in VerticalMeasure/MeasureUnitCode. \cr
#' VerticalMeasure/MeasureUnitCode \tab character \tab	The code that represents the unit for measuring the item. \cr
#' VerticalAccuracyMeasure/MeasureValue * \tab character \tab	The vertical measure of the relative accuracy of the latitude and longitude coordinates. Measure value is given in the units stored in VerticalAccuracyMeasure/MeasureUnitCode. \cr
#' VerticalAccuracyMeasure/MeasureUnitCode * \tab character \tab	The code that represents the unit for measuring the item. \cr
#' VerticalCollectionMethodName \tab character \tab	The name that identifies the method used to collect the vertical measure (i.e. the altitude) of a reference point. \cr
#' VerticalCoordinateReferenceSystemDatumName \tab character \tab	The name of the reference datum used to determine the vertical measure (i.e., the altitude). \cr
#' CountryCode \tab character \tab	A code designator used to identify a primary geopolitical unit of the world. \cr
#' StateCode \tab character \tab	A code designator used to identify a principal administrative subdivision of the United States, Canada, or Mexico. \cr
#' CountyCode \tab character \tab	A code designator used to identify a U.S. county or county equivalent. \cr
#' AquiferName * \tab character \tab 	Name of the aquifer in which the well is completed. \cr
#' FormationTypeText * \tab character \tab	Name of the primary formation or soils unit, in which the well is completed. \cr
#' AquiferTypeName * \tab character \tab	The type of aquifer, such as confined or unconfined. \cr
#' ConstructionDateText * \tab character \tab	Date of construction when well was completed. May be year only. \cr
#' WellDepthMeasure/MeasureValue * \tab character \tab	Depth below land surface datum (LSD) to the bottom of the hole on completion of drilling. Measure value is given in the units stored in WellDepthMeasure/MeasureUnitCode. \cr
#' WellDepthMeasure/MeasureUnitCode * \tab character \tab	The code that represents the unit for measuring the item. \cr
#' WellHoleDepthMeasure/MeasureValue * \tab character \tab	Depth below land surface datum (LSD) to the bottom of the hole on completion of drilling. Measure value is given in the units stored in WellHoleDepthMeasure/MeasureUnitCode. \cr
#' WellHoleDepthMeasure/MeasureUnitCode * \tab character \tab	The code that represents the unit for measuring the item. \cr
#' queryTime \tab POSIXct \tab Query time \cr
#' }
#' * element is only in NWIS
#'
#' @export
#' @seealso whatNWISdata
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#'
#' site1 <- whatWQPsites(siteid = "USGS-01594440")
#'
#' type <- "Stream"
#' sites <- whatWQPsites(
#'   countycode = "US:55:025",
#'   characteristicName = "Phosphorus",
#'   siteType = type
#' )
#' }
whatWQPsites <- function(...) {
  values <- readWQPdots(...)

  values <- values$values

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }

  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))

  baseURL <- drURL("Station", arg.list = values)

  baseURL <- appendDrURL(baseURL, mimeType = "tsv")

  retval <- importWQP(baseURL, zip = values["zip"] == "yes")

  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- baseURL

  return(retval)
}


#' Summary of Data Available from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This
#' function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on
#' \url{https://www.waterqualitydata.us/webservices_documentation}.
#' The information returned from this function describes the
#' available data at the WQP sites, and some metadata on the sites themselves.
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation}
#'  for a complete list of options. A list of arguments can also be supplied.

#' @return A data frame with at least the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#'  "Provider" \tab character \tab Providing database.  \cr
#'  "MonitoringLocationIdentifier" \tab character \tab	A designator used to
#' describe the unique name, number, or code assigned to identify
#' the monitoring location.\cr
#'  "YearSummarized" \tab numeric \tab The year of the summary \cr
#'  "CharacteristicType" \tab character \tab CharacteristicType  \cr
#'  "CharacteristicName" \tab character \tab	The object, property, or substance
#' which is evaluated or enumerated by either a direct field measurement,
#' a direct field observation, or by laboratory analysis of material
#' collected in the field.\cr
#'  "ActivityCount" \tab numeric \tab The number of times the location was sampled \cr
#'  "ResultCount" \tab numeric \tab The number of individual data results. \cr
#'  "LastResultSubmittedDate" \tab Date \tab Date when data was last submitted. \cr
#'  "OrganizationIdentifier" \tab character \tab  A designator used to uniquely
#' identify a unique business establishment within a context.\cr
#'  "OrganizationFormalName" \tab character \tab  The legal designator
#' (i.e. formal name) of an organization.\cr
#'  "MonitoringLocationName \tab character \tab MonitoringLocationName \cr
#'  "MonitoringLocationTypeName" \tab character \tab MonitoringLocationTypeName \cr
#'  "ResolvedMonitoringLocationTypeName" \tab character \tab  \cr
#'  "HUCEightDigitCode" \tab character \tab 8-digit HUC id. \cr
#'  "MonitoringLocationUrl" \tab character \tab URL to monitoring location. \cr
#'  "CountyName" \tab character \tab County of sampling location. \cr
#'  "StateName" \tab character \tab State of sampling location. \cr
#'  "MonitoringLocationLatitude"  \tab numeric \tab latitude of sampling
#'  location. \cr
#'  "MonitoringLocationLongitude" \tab numeric \tab longitude of sampling
#'  location. \cr
#' }
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' # Summary of a single site for the last 5 years:
#' site_5 <- readWQPsummary(
#'   siteid = "USGS-07144100",
#'   summaryYears = 5
#' )
#'
#' # Summary of a single site for the full period of record:
#' site_all <- readWQPsummary(
#'   siteid = "USGS-07144100",
#'   summaryYears = "all"
#' )
#'
#' # Summary of the data available from streams in a single county:
#' dane_county_data <- readWQPsummary(
#'   countycode = "US:55:025",
#'   summaryYears = 5,
#'   siteType = "Stream"
#' )
#'
#' # Summary of the data all available from lakes in a single county:
#' lake_sites <- readWQPsummary(
#'   siteType = "Lake, Reservoir, Impoundment",
#'   countycode = "US:55:025"
#' )
#'
#' # Summary of the data available for the last 5 years in New Jersey:
#' state1 <- readWQPsummary(
#'   statecode = "NJ",
#'   summaryYears = 5,
#'   siteType = "Stream"
#' )
#' }
readWQPsummary <- function(...) {
  values <- readWQPdots(...)

  values <- values$values

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }

  if (!"dataProfile" %in% names(values)) {
    values[["dataProfile"]] <- "periodOfRecord"
  }

  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))

  baseURL <- drURL("SiteSummary", arg.list = values)
  baseURL <- appendDrURL(baseURL, mimeType = "csv")

  withCallingHandlers(
    {
      retval <- importWQP(baseURL, zip = values["zip"] == "yes", csv = TRUE)
    },
    warning = function(w) {
      if (any(grepl("Number of rows returned not matched in header", w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- baseURL

  return(retval)
}
