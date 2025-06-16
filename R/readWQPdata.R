#' General Data Import from Water Quality Portal
#'
#' Imports data from Water Quality Portal web service. This function gets the data from here:
#' <https://www.waterqualitydata.us>.
#' 
#' This function uses \dots as a query input, which can be very flexible, but also 
#' has a steeper learning curve. For a quick overview, scroll down to the Examples
#' in this help file to see many query options. 
#' 
#' @details
#'  
#' There are currently 10 legacy options
#' for data provided by the Water Quality Portal:
#'  
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
#' There are 4 WQX3 options. These are still in-development,
#' and should be used with caution.
#'  
#' \tabular{llll}{
#' WQP Radio Button \tab service argument \tab Base URL \tab dataProfile \cr 
#' Monitoring Locations \tab StationWQX3 \tab /wqx3/Station/search \tab \cr
#' Full Physical Chemical \tab ResultWQX3 \tab /wqx3/Result/search \tab fullPhysChem \cr
#' Narrow \tab ResultWQX3 \tab /wqx3/Result/search \tab narrow \cr
#' Basic Physical Chemical \tab ResultWQX3 \tab /wqx3/Result/search \tab basicPhysChem \cr
#' Sampling Activity \tab ActivityWQX3 \tab /wqx3/Activity/search \cr
#' }
#' 
#'
#' @param \dots see <https://www.waterqualitydata.us/webservices_documentation> for a complete list of options.
#' A list of arguments can also be supplied. For more information see the above 
#' description for this help file. One way to figure out how to construct a WQP query is to go to the "Advanced" 
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
#' See also  `OlsonNames()`
#' for more information on time zones.
#' @param ignore_attributes logical to choose to ignore fetching site and status
#' attributes. Default is `FALSE`.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
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
#' 
#' # Legacy:
#' nameToUse <- "pH"
#' pHData <- readWQPdata(siteid = "USGS-04024315", 
#'                       characteristicName = nameToUse)
#' ncol(pHData)
#' attr(pHData, "siteInfo")
#' attr(pHData, "queryTime")
#' attr(pHData, "url")
#' 
#' # WQX3:
#' pHData_wqx3 <- readWQPdata(siteid = "USGS-04024315", 
#'                            characteristicName = nameToUse,
#'                            service = "ResultWQX3",
#'                            dataProfile = "basicPhysChem")
#' attr(pHData_wqx3, "url")
#'
#' # More examples:
#' # querying by county
#' DeWitt <- readWQPdata(
#'   statecode = "Illinois",
#'   countycode = "DeWitt",
#'   characteristicName = "Nitrogen"
#' )
#' 
#' attr(DeWitt, "url")
#' 
#' DeWitt_wqx3 <- readWQPdata(
#'    statecode = "Illinois",
#'    countycode = "DeWitt",
#'    characteristicName = "Nitrogen",
#'    service = "ResultWQX3",
#'    dataProfile = "basicPhysChem",  
#'    ignore_attributes = TRUE)
#' 
#' attr(DeWitt_wqx3, "url")
#' 
#' # Data profile: "Sampling Activity"
#' activity <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   service = "Activity"
#' )
#' attr(activity, "url")
#' 
#' # activity_wqx3 <- readWQPdata(
#' #   siteid = "USGS-04024315",
#' #   service = "ActivityWQX3"
#' # )
#' # attr(activity_wqx3, "url")
#' 
#' Dane_activity <- readWQPdata(
#'   statecode = "Wisconsin",
#'   countycode = "Dane",
#'   startDateLo = "2023-01-01",
#'   startDateHi = "2023-12-31",
#'   service = "Activity"
#' )
#' attr(Dane_activity, "url")
#' 
#' # Dane_activity_wqx3 <- readWQPdata(
#' #   statecode = "Wisconsin",
#' #   countycode = "Dane",
#' #   startDateLo = "2023-01-01",
#' #   startDateHi = "2023-12-31",
#' #   service = "ActivityWQX3"
#' # )
#' # attr(Dane_activity_wqx3, "url")
#' 
#' ########################################################
#' # Additional examples:
#'
#'  
#' # Data profiles: "Organization Data" 
#' org_data <- readWQPdata(
#'   statecode = "WI",
#'   countycode = "Dane",
#'   service = "Organization"
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
#' # Data profiles: "Sample Results (physical/chemical metadata)" 
#' samp_data <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "resultPhysChem",
#'   service = "Result"
#' )
#'
#' # Data profiles: "Sample Results (biological metadata)"
#' samp_bio <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "biological",
#'   service = "Result"
#' )
#' 
#'
#' # Data profiles: "Sample Results (narrow)" 
#' samp_narrow <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   service = "Result",
#'   dataProfile = "narrowResult"
#' )
#'
#' # samp_narrow_wqx3 <- readWQPdata(
#' #   siteid = "USGS-04024315",
#' #   service = "ResultWQX3",
#' #   dataProfile = "narrow"
#' # )
#'
#'
#' # Data profiles: "Sampling Activity"  
#' samp_activity <- readWQPdata(
#'   siteid = "USGS-04024315",
#'   dataProfile = "activityAll",
#'   service = "Activity"
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
#' # other options:
#' Phosphorus <- readWQPdata(
#'   statecode = "WI", countycode = "Dane", 
#'   characteristicName = "Phosphorus",
#'   startDateLo = "2023-01-01",
#'   ignore_attributes = TRUE,
#'   convertType = FALSE
#' )
#' 
#' rawPHsites_legacy <- readWQPdata(siteid = c("USGS-05406450", "USGS-05427949", "WIDNR_WQX-133040"),
#'                         characteristicName = "pH",
#'                         service = "Result",
#'                         dataProfile = "narrowResult" )
#' 
#' # rawPHsites <- readWQPdata(siteid = c("USGS-05406450", "USGS-05427949", "WIDNR_WQX-133040"),
#' #                           characteristicName = "pH",
#' #                           service = "ResultWQX3",
#' #                           dataProfile = "narrow" )
#' 
#' }
readWQPdata <- function(...,
                        service = "Result",
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
  values <- valuesList[["values"]]
  
  baseURL <- httr2::request(pkg.env[[service]])
  POST <- FALSE
  
  if(!legacy){
    if(service == "ResultWQX3" & !"dataProfile" %in% names(values)){
      baseURL <- httr2::req_url_query(baseURL, 
                                      dataProfile = "fullPhysChem")
    }
    baseURL <- httr2::req_url_query(baseURL, !!!values, 
                                    .multi = "explode")
  } else {
    if("siteid" %in% names(values)){
      if(length(values[["siteid"]]) > 1){
        sites <- values[["siteid"]]
        POST <- nchar(paste0(sites, collapse = "")) > 2048
        
        baseURL <- get_or_post(baseURL, 
                               POST = POST,
                               siteid = sites,
                               .multi = function(x) paste0(x, collapse = ";"))
        values <- values[names(values) != "siteid"]
      }
    }
    baseURL <- get_or_post(baseURL, 
                           POST = POST,
                           !!!values, 
                           .multi = "explode")
  }
  
  if (querySummary) {
    retquery <- getQuerySummary(baseURL)
    return(retquery)
  } else {
    retval <- importWQP(baseURL,
                        tz = tz,
                        convertType = convertType
    )
    
    if(is.null(retval)){
      return(NULL)
    }
    
    attr(retval, "legacy") <- legacy
    
    if (!all(is.na(retval)) && 
        !ignore_attributes && 
        !service %in% c("Station", "StationWQX")) {
      params <- convertLists(...)
      params <- params[!names(params) %in% c("dataProfile", "service")]
      retval <- create_WQP_attributes(retval, params)
    } 
    
    attr(retval, "url") <- baseURL$url
    
    if(legacy){
      wqp_message()
    } else {
      wqp_message_beta()
      attr(retval, "wqp-request-id") <- attr(retval, "headerInfo")$`wqp-request-id`
    }
    
    return(retval)
  }
}


create_WQP_attributes <- function(retval, ...){
  
  col_legacy <- c("CharacteristicName", #legacy
                  "ResultMeasure.MeasureUnitCode", 
                  "ResultSampleFractionText")
  col_wqx3 <- c("Result_Characteristic", #wqx3
                "Result_MeasureUnit", 
                "Result_SampleFraction")
  if (all(col_legacy %in% names(retval))) {
    retvalVariableInfo <- retval[, col_legacy]
    retvalVariableInfo <- unique(retvalVariableInfo)
    names(retvalVariableInfo) <- c("characteristicName",
                                   "param_units",
                                   "valueType")
    
    attr(retval, "variableInfo") <- retvalVariableInfo
  } else if(all(col_wqx3 %in% names(retval))){
    retvalVariableInfo <- retval[, col_wqx3]
    retvalVariableInfo <- unique(retvalVariableInfo)
    names(retvalVariableInfo) <- c("characteristicName",
                                   "param_units",
                                   "valueType")
    
    attr(retval, "variableInfo") <- retvalVariableInfo
  }
  
  siteInfo <- suppressWarnings(whatWQPsites(..., legacy = attr(retval, "legacy")))
  attr(retval, "siteInfo") <- siteInfo
  
  if(!attr(retval, "legacy")){
    attr(retval, "headerInfo") <- wqp_check_status(attr(retval, "headerInfo")$`wqp-request-id`)
    attr(retval, "queryTime") <- as.POSIXct(attr(retval, "headerInfo")[["requestStartTime"]],
                                            format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  } else {
    attr(retval, "queryTime") <- Sys.time()
  }
  
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
#' rawPcode <- readWQPqw("USGS-01594440", "01075", 
#'                       ignore_attributes = TRUE, legacy = FALSE)
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

