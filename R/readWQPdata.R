#' General Data Import from Water Quality Portal
#'
#' Imports data from Water Quality Portal web service. This function gets the data from here:
#' \url{https://www.waterqualitydata.us}.
#' 
#' This function uses \dots as a query input, which can be very flexible, but also 
#' has a steeper learning curve. For a quick overview, scroll down to the Examples
#' in this help file to see many query options. 
#' 
#' @details
#'  
#' There are currently 10 legacy and 4 modern WQX options
#' for data provided by the Water Quality Portal:
#'  
#' WQX:
#' \tabular{llll}{
#' WQP Radio Button \tab service argument \tab Base URL \tab dataProfile \cr 
#' Monitoring Locations \tab StationWQX3 \tab /wqx3/Station/search \tab \cr
#' Full Physical Chemical \tab ResultWQX3 \tab /wqx3/Result/search \tab fullPhysChem \cr
#' Narrow \tab ResultWQX3 \tab /wqx3/Result/search \tab narrow \cr
#' Basic Physical Chemical \tab ResultWQX3 \tab /wqx3/Result/search \tab basicPhysChem \cr
#' Sampling Activity \tab ActivityWQX3 \tab /wqx3/Activity/search \cr
#' }
#' 
#' Legacy:
#' \tabular{lll}{
#' WQP Radio Button \tab service argument \tab Base URL  \cr
#' Sample Results \tab Result \tab /data/Result/search \cr
#' Site Data Only \tab Station \tab /data/Station/search \cr
#' Sampling Activity \tab Activity \tab /data/Activity/search \cr
#' Sampling Activity Metrics \tab ActivityMetric \tab /data/ActivityMetric/search \cr
#' Site Summary (not advertised on WQP)  \tab SiteSummary \tab /data/summary/monitoringLocation/search \cr
#' Project Data \tab Project \tab /data/Project/search \cr
#' Project Monitoring Location Weighting Data \tab ProjectMonitoringLocationWeighting \tab /data/ProjectMonitoringLocationWeighting/search \cr
#' Result Detection Quantitation Limit Data \tab ResultDetectionQuantitationLimit \tab /data/ResultDetectionQuantitationLimit/search \cr
#' Biological Habitat Metrics \tab BiologicalMetric \tab /data/BiologicalMetric/search \cr
#' Organization Data \tab Organization \tab /data/Organization/search \cr
#' }
#' 
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options.
#' A list of arguments can also be supplied. For more information see the above 
#' description for this help file. If no "service" argument is supplied, it
#' will default to "ResultWQX3". One way to figure out how to construct a WQP query is to go to the "Advanced" 
#' form in the Water Quality Portal. Use the form to discover what parameters are available. Once the query is 
#' set in the form, scroll down to the "Query URL". You will see the parameters
#' after "https://www.waterqualitydata.us/#". For example, if you chose "Nutrient"
#' in the Characteristic Group dropdown, you will see characteristicType=Nutrient
#' in the Query URL. The corresponding argument for dataRetrieval is
#' characteristicType = "Nutrient". dataRetrieval users do not need to include
#' mimeType, and providers is optional (these arguments are picked automatically).
#' @param service character. See Details for more information.
#' @param querySummary logical to only return the number of records and unique sites that
#' will be returned from this query. Choosing TRUE is deprecated, readWQPsummary 
#' is recommended instead.
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
#' @param ignore_attributes logical to choose to ignore fetching site and status
#' attributes. Default is \code{FALSE}.
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
#' headerInfo \tab data.frame \tab A data frame returned from the WQP status service \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' nameToUse <- "pH"
#' pHData <- readWQPdata(siteid = "USGS-04024315", 
#'                       characteristicName = nameToUse)
#' ncol(pHData)
#' attr(pHData, "url")
#' attr(pHData, "siteInfo")
#' attr(pHData, "headerInfo")[["dataProviders"]]
#' attr(pHData, "queryTime")
#'
#' # dataProfile = Basic Physical Chemical
#' pHData_basic <- readWQPdata(siteid = "USGS-04024315", 
#'                       characteristicName = nameToUse,
#'                       dataProfile = "basicPhysChem")
#' attr(pHData_basic, "url") 
#' ncol(pHData_basic)
#'
#' # dataProfile = Narrow
#' pHData_narrow <- readWQPdata(siteid = "USGS-04024315", 
#'                       characteristicName = nameToUse,
#'                       dataProfile = "narrow")
#' attr(pHData_narrow, "url") 
#' ncol(pHData_narrow)
#' 
#' # Data profiles: "Site Data Only"
#' site_data <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "StationWQX3"
#' )
#'
#' # More examples:
#' # querying by county
#' DeWitt <- readWQPdata(
#'   statecode = "Illinois",
#'   countycode = "DeWitt",
#'   characteristicName = "Nitrogen"
#' )
#' 
#' # Data profile: "Sampling Activity"
#' activity <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   service = "ActivityWQX3"
#' )
#' 
#' Dane_activity <- readWQPdata(
#'   statecode = "Wisconsin",
#'   countycode = "Dane",
#'   startDateLo = "2023-01-01",
#'   startDateHi = "2023-12-31",
#'   service = "ActivityWQX3"
#' )
#' 
#' ########################################################
#' # Legacy examples:
#'
#' pHData_legacy <- readWQPdata(siteid = "USGS-04024315", 
#'                       characteristicName = nameToUse,
#'                       service = "Result",
#'                       dataProfile = "narrowResult")
#' attr(pHData_legacy, "url")
#' 
#' # Data profiles: "Organization Data" (legacy)
#' org_data <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "Organization"
#' )
#'
#' # Data profiles: "Project Data"  (legacy)
#' project_data <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "Project"
#' )
#'
#' # Data profiles: "Project Monitoring Location Weighting Data"  (legacy)
#' proj_mlwd <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "ProjectMonitoringLocationWeighting"
#' )
#'
#' # Data profiles: "Sample Results (physical/chemical metadata)"  (legacy)
#' samp_data <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "resultPhysChem",
#'   service = "Result"
#' )
#'
#' # Data profiles: "Sample Results (biological metadata)"  (legacy)
#' samp_bio <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "biological",
#'   service = "Result"
#' )
#'
#' # Data profiles: "Sample Results (narrow)" (legacy)
#' samp_narrow <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   service = "Result",
#'   dataProfile = "narrowResult"
#' )
#'
#' # Data profiles: "Sampling Activity"  (legacy)
#' samp_activity <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "activityAll",
#'   service = "Activity"
#' )
#'
#' # Data profile: "Sampling Activity Metrics"  (legacy)
#' act_metrics <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "ActivityMetric"
#' )
#'
#' # Data profile: "Result Detection Quantitation Limit Data"  (legacy)
#' dl_data <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   service = "ResultDetectionQuantitationLimit"
#' )
#'
#' # other options:
#' Phosphorus <- readWQPdata(
#'   statecode = "WI", countycode = "Dane", 
#'   characteristicName = "Phosphorus",
#'   startDateLo = "2023-01-01",
#'   ignore_attributes = TRUE,
#'   convertType = FALSE
#' )
#' }
readWQPdata <- function(...,
                        service = "ResultWQX3",
                        querySummary = FALSE,
                        tz = "UTC",
                        ignore_attributes = FALSE,
                        convertType = TRUE) {
  
  tz <- match.arg(tz, OlsonNames())

  service <- match.arg(service, c("Result", "Station", "Activity",
                                  "ActivityMetric", "SiteSummary",
                                  "Project", "ProjectMonitoringLocationWeighting",
                                  "ResultDetectionQuantitationLimit",
                                  "BiologicalMetric", "Organization",
                                  "ResultWQX3", "StationWQX3", "ActivityWQX3"),
                       several.ok = FALSE)
  
  legacy <- is_legacy(service)
  
  valuesList <- readWQPdots(..., legacy = legacy)
  
  values <- sapply(valuesList$values, function(x) utils::URLencode(x, reserved = TRUE))

  baseURL <- drURL(service, arg.list = values)

  baseURL <- appendDrURL(baseURL, mimeType = "csv")
  
  wqp_message_now(service)
  
  if(!legacy){
    if(service == "ResultWQX3" & !"dataProfile" %in% names(values)){
      baseURL <- appendDrURL(baseURL, dataProfile = "fullPhysChem")
    }
  } 

  if (querySummary) {
    retquery <- getQuerySummary(baseURL)
    return(retquery)
  } else {
    retval <- importWQP(baseURL,
      tz = tz,
      convertType = convertType
    )
    
    attr(retval, "legacy") <- legacy
    
    if(!legacy){
      attr(retval, "wqp-request-id") <- attr(retval, "headerInfo")$`wqp-request-id`
    }

    if (!all(is.na(retval)) && !ignore_attributes) {
      params <- list(...)
      params <- params[!names(params) %in% c("dataProfile", "service")]
      retval <- create_WQP_attributes(retval, params)

    } 

    attr(retval, "url") <- baseURL

    return(retval)
  }
}


create_WQP_attributes <- function(retval, ...){

  siteInfo <- suppressWarnings(whatWQPsites(...))
  
  attr(retval, "siteInfo") <- siteInfo
  
  if(!attr(retval, "legacy")){
    attr(retval, "headerInfo") <- wqp_check_status(attr(retval, "headerInfo")$`wqp-request-id`)
    attr(retval, "queryTime") <- as.POSIXct(attr(retval, "headerInfo")[["requestStartTime"]],
                                            format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
  } else {
    attr(retval, "queryTime") <- Sys.time()
  }
  
  #If WQP adds a parameter metadata service/files, we could add that here.
  return(retval)
}

#' Get WQP service metadata
#' 
#' The information from this request is only available for a 
#' limited time after the original query from the WQP. In the 
#' readWQPdata and readWQPqw functions, the results from this
#' function will be attached as an attribute to the data.
#' 
#' @param wqp_request_id A character returned from the header
#' of a WQP request.
#' @return a list generated from the WQP describing what data
#' was returned.
#' @export
#' 
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' rawPcode <- readWQPqw("USGS-01594440", "01075", ignore_attributes = TRUE)
#' headerInfo <- attr(rawPcode, "headerInfo")
#' wqp_request_id <- headerInfo$`wqp-request-id`
#' count_info <- wqp_check_status(wqp_request_id)
#' count_info[["dataProviders"]]
#' }
wqp_check_status <- function(wqp_request_id){
  
  id_url <- paste0(pkg.env[["status"]], wqp_request_id)
  counts_list <- get_nldi_sources(id_url)
  return(counts_list)
  
}

