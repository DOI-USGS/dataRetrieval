#'
#' Format and organize WQP arguments that are passed in as \code{...}.
#'
#' @keywords internal
readWQPdots <- function(..., legacy = TRUE) {
  
  if (length(list(...)) == 0) {
    stop("No arguments supplied")
  }

  matchReturn <- convertLists(...)

  if ("service" %in% names(matchReturn)) {
    service <- matchReturn$service
    matchReturn$service <- NULL
  } else {
    if(legacy){
      service <- "Result"
    } else {
      service <- "ResultWQX3"
    }
    
  }
  
  if ("dataProfile" %in% names(matchReturn)) {
    profile <- matchReturn$dataProfile
    if (profile == "activityAll") {
      service <- "ActivityWQX3"
      matchReturn$service <- NULL
    } else if (profile %in% c("resultPhysChem","biological","narrowResult")) {
      service <- "Result"
      matchReturn$service <- NULL
    } else if(profile %in% c("fullPhysChem", "narrow", "basicPhysChem")){
      service <- "ResultWQX3"
      matchReturn$service <- NULL      
    } 
  } 
  
  match.arg(service, c(
    "Result", "Station", "Activity", "Organization",
    "ActivityMetric", "SiteSummary",
    "Project", "ProjectMonitoringLocationWeighting",
    "ResultDetectionQuantitationLimit", "BiologicalMetric",
    "ResultWQX3", "StationWQX3", "ActivityWQX3"
  ))
  
  names(matchReturn)[names(matchReturn) == "bbox"] <- "bBox"
    
  bbox <- "bBox" %in% names(matchReturn)
  if(bbox){
    matchReturn["bBox"] <- sapply(matchReturn["bBox"], function(x) as.character(paste0(eval(x), collapse = ",")))
  }

  values <- matchReturn
  values <- checkWQPdates(values)

  names(values)[names(values) == "siteNumber"] <- "siteid"
  names(values)[names(values) == "siteNumbers"] <- "siteid"
  names(values)[names(values) == "parameterCd"] <- "pCode"
  names(values)[names(values) == "USGSPCode"] <- "pCode"
  

  names(values)[names(values) == "stateCd"] <- "statecode"
  if ("statecode" %in% names(values)) {
    stCd <- values["statecode"]
    stCdPrefix <- "US:"
    if (!grepl(stCdPrefix, stCd)) {
      values["statecode"] <- stateCdLookup(stCd, "fips")
    }
  }

  names(values)[names(values) == "countyCd"] <- "countycode"
  if (all(c("countycode", "statecode") %in% names(values))) {
    stCd <- gsub("US:", "", values["statecode"])
    # This will error if more than 1 state is requested
    # It's possible that someone could request more than one state
    # in WQP, but if they also then request county codes,
    # it gets really confusing, and the WQP developers don't recommend.
    values["countycode"] <- countyCdLookup(stCd, values["countycode"], "fips")
  }
  
  if(!"mimeType" %in% names(values)){
    values["mimeType"] <- "csv"
  }
  
  if(legacy & !("count" %in% names(values))){
    values["count"] <- "no"
  }
  
  return_list <- list()
  return_list["values"] <- list(values)
  return_list["service"] <- service
  return(return_list)
}

