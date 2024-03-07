#' General Data Import from Water Quality Portal
#'
#' Imports data from Water Quality Portal web service. This function gets the data from here:
#' \url{https://www.waterqualitydata.us}.
#' because it allows for other agencies rather than the USGS. 
#' 
#' This function uses \dots as a query input, which can be very flexible, but also 
#' has a steeper learning curve. For a quick overview, scroll down to the Examples
#' in this help file to see many query options. 
#' 
#' 
#' There are currently 10 "services" provided by the Water Quality Portal:
#' \tabular{ll}{
#' Name \tab Base URL \cr
#' Result (default) \tab "https://www.waterqualitydata.us/data/Result/search" \cr
#' Station \tab  "https://www.waterqualitydata.us/data/Station/search" \cr
#' Activity \tab "https://www.waterqualitydata.us/data/Activity/search" \cr
#' ActivityMetric \tab "https://www.waterqualitydata.us/data/ActivityMetric/search" \cr
#' SiteSummary \tab "https://www.waterqualitydata.us/data/summary/monitoringLocation/search" \cr
#' Project \tab "https://www.waterqualitydata.us/data/Project/search" \cr
#' ProjectMonitoringLocationWeighting \tab "https://www.waterqualitydata.us/data/ProjectMonitoringLocationWeighting/search" \cr
#' ResultDetectionQuantitationLimit \tab "https://www.waterqualitydata.us/data/ResultDetectionQuantitationLimit/search" \cr
#' BiologicalMetric \tab "https://www.waterqualitydata.us/data/BiologicalMetric/search" \cr
#' Organization \tab "https://www.waterqualitydata.us/data/Organization/search" \cr
#' }
#' 
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options.
#' A list of arguments can also be supplied. For more information see the above 
#' description for this help file. If no "service" argument is supplied, it
#' will default to "Result". One way to figure out how to construct a WQP query is to go to the "Advanced" 
#' form in the Water Quality Portal:
#' \url{https://www.waterqualitydata.us/#mimeType=csv&providers=NWIS&providers=STORET}
#' Use the form to discover what parameters are available. Once the query is 
#' set in the form, scroll down to the "Query URL". You will see the parameters
#' after "https://www.waterqualitydata.us/#". For example, if you chose "Nutrient"
#' in the Characteristic Group dropdown, you will see characteristicType=Nutrient
#' in the Query URL. The corresponding argument for dataRetrieval is
#' characteristicType = "Nutrient". dataRetrieval users do not need to include
#' mimeType, zip, and providers is optional (these arguments are picked automatically).
#' 
#' @param querySummary logical to only return the number of records and unique sites that
#' will be returned from this query. 
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the
#' data's provided tz_cd column.
#' Possible values to provide are "America/New_York","America/Chicago",
#' "America/Denver","America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings
#' time: "America/Honolulu",
#' "America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla".
#' See also  \code{OlsonNames()}
#' for more information on time zones.
#' @param ignore_attributes logical to choose to ignore fetching site and parameter
#' attributes. Default is \code{FALSE}.
#' @param checkHeader logical, defaults to \code{FALSE}. If \code{TRUE}, the code
#' will check that the curl header response for number of rows matches the actual
#' number of rows. During transition to WQX 3.0 profiles, it's unclear if
#' the counts will be correct.
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @keywords data import WQP web service
#' @return A data frame, the specific columns will depend on the "service" and/or "dataProfile".
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
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' nameToUse <- "pH"
#' pHData <- readWQPdata(siteid = "USGS-04024315", characteristicName = nameToUse)
#' pHData_summary <- readWQPdata(
#'   bBox = c(-90.10, 42.67, -88.64, 43.35),
#'   characteristicName = nameToUse, querySummary = TRUE
#' )
#' startDate <- as.Date("2013-01-01")
#' secchi.names <- c(
#'   "Depth, Secchi disk depth",
#'   "Depth, Secchi disk depth (choice list)",
#'   "Secchi Reading Condition (choice list)",
#'   "Water transparency, Secchi disc"
#' )
#' args <- list(
#'   "startDateLo" = startDate,
#'   "startDateHi" = "2013-12-31",
#'   statecode = "WI",
#'   characteristicName = secchi.names
#' )
#'
#' wqp.data <- readWQPdata(args)
#'
#' args_2 <- list(
#'   "startDateLo" = startDate,
#'   "startDateHi" = "2013-12-31",
#'   statecode = "WI",
#'   characteristicName = secchi.names,
#'   querySummary = TRUE
#' )
#'
#' wqp.summary <- readWQPdata(args_2)
#'
#' arg_3 <- list(
#'   "startDateLo" = startDate,
#'   "startDateHi" = "2013-12-31"
#' )
#' arg_4 <- list(
#'   statecode = "WI",
#'   characteristicName = secchi.names
#' )
#' wqp.summary <- readWQPdata(arg_3, arg_4, querySummary = TRUE)
#' wqp.summary_WI <- readWQPdata(arg_3,
#'   statecode = "WI",
#'   characteristicName = secchi.names,
#'   querySummary = TRUE
#' )
#'
#' # querying by county
#' DeWitt <- readWQPdata(
#'   statecode = "Illinois",
#'   countycode = "DeWitt",
#'   characteristicName = "Nitrogen"
#' )
#'
#' # Data profiles: "Organization Data"
#' org_data <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "Organization"
#' )
#'
#' # Data profiles: "Site Data Only"
#' site_data <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "Station"
#' )
#'
#' # Data profiles: "Project Data"
#' project_data <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "Project"
#' )
#'
#' # Data profiles: "Project Monitoring Location Weighting Data"
#' proj_mlwd <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "ProjectMonitoringLocationWeighting"
#' )
#'
#' # Data profiles: "Sample Results (physical/chemical metadata)":
#' samp_data <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "resultPhysChem"
#' )
#'
#' # Data profiles: "Sample Results (biological metadata)"
#' samp_bio <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "biological"
#' )
#'
#' # Data profiles: "Sample Results (narrow)"
#' samp_narrow <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "narrowResult"
#' )
#'
#' # Data profiles: "Sampling Activity"
#' samp_activity <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "activityAll"
#' )
#'
#' # Data profile: "Sampling Activity Metrics"
#' act_metrics <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "ActivityMetric"
#' )
#'
#' # Data profile: "Result Detection Quantitation Limit Data"
#' dl_data <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   service = "ResultDetectionQuantitationLimit"
#' )
#'
#' Phosphorus <- readWQPdata(
#'   statecode = "WI", countycode = "Dane", 
#'   characteristicName = "Phosphorus",
#'   startDateLo = "2020-01-01",
#'   convertType = FALSE
#' )
#' }
readWQPdata <- function(...,
                        querySummary = FALSE,
                        tz = "UTC",
                        ignore_attributes = FALSE,
                        convertType = TRUE,
                        checkHeader = FALSE) {
  tz <- match.arg(tz, OlsonNames())

  wqp_message()
  
  valuesList <- readWQPdots(...)

  service <- valuesList$service
  
  service <- match.arg(service, c("Result", "Station", "Activity",
                                  "ActivityMetric", "SiteSummary",
                                  "Project", "ProjectMonitoringLocationWeighting",
                                  "ResultDetectionQuantitationLimit",
                                  "BiologicalMetric", "Organization"),
                       several.ok = FALSE)

  values <- sapply(valuesList$values, function(x) utils::URLencode(x, reserved = TRUE))

  baseURL <- drURL(service, arg.list = values)

  baseURL <- appendDrURL(baseURL, mimeType = "tsv")

  if (querySummary) {
    retquery <- getQuerySummary(baseURL)
    return(retquery)
  } else {
    retval <- importWQP(baseURL,
      zip = values["zip"] == "yes",
      tz = tz,
      convertType = convertType,
      checkHeader = checkHeader
    )

    if (!all(is.na(retval)) && !ignore_attributes) {
      siteInfo <- suppressWarnings(whatWQPsites(..., service = "Station"))

      if (all(c(
        "MonitoringLocationName",
        "OrganizationIdentifier",
        "MonitoringLocationIdentifier",
        "LatitudeMeasure",
        "LongitudeMeasure",
        "HUCEightDigitCode"
      ) %in% names(siteInfo))) {
        siteInfoCommon <- data.frame(
          station_nm = siteInfo$MonitoringLocationName,
          agency_cd = siteInfo$OrganizationIdentifier,
          site_no = siteInfo$MonitoringLocationIdentifier,
          dec_lat_va = siteInfo$LatitudeMeasure,
          dec_lon_va = siteInfo$LongitudeMeasure,
          hucCd = siteInfo$HUCEightDigitCode,
          stringsAsFactors = FALSE
        )

        siteInfo <- cbind(siteInfoCommon, siteInfo)
      }

      attr(retval, "siteInfo") <- siteInfo

      if (all(c(
        "CharacteristicName",
        "ResultMeasure.MeasureUnitCode",
        "ResultSampleFractionText"
      ) %in% names(retval))) {
        retvalVariableInfo <- retval[, c(
          "CharacteristicName",
          "ResultMeasure.MeasureUnitCode",
          "ResultSampleFractionText"
        )]
        retvalVariableInfo <- unique(retvalVariableInfo)

        variableInfo <- data.frame(
          characteristicName = retval$CharacteristicName,
          param_units = retval$ResultMeasure.MeasureUnitCode,
          valueType = retval$ResultSampleFractionText,
          stringsAsFactors = FALSE
        )

        attr(retval, "variableInfo") <- variableInfo
      }
    } else {
      if (!ignore_attributes) {
        message("The following url returned no data:\n")
        message(baseURL)
      }
    }
    attr(retval, "queryTime") <- Sys.time()
    attr(retval, "url") <- baseURL

    return(retval)
  }
}
