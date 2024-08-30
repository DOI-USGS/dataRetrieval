pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {
  suppressMessages(setAccess("public"))
  pkg.env$nldi_base <- "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
  pkg.env$local_sf <- requireNamespace("sf", quietly = TRUE)
}


#' Is this a dataRetrieval user
#'
#' Reveals if this is a user or not
#' @export
#' @examples
#' is_dataRetrieval_user()
is_dataRetrieval_user <- function() {
  interactive() ||
    !identical(Sys.getenv("CI"), "") ||
    identical(Sys.getenv("NOT_CRAN"), "true")
}


wqp_message_only_legacy <- function(){
  message("NEWS: Legacy profile requested, and no equivalent/similar WQX 3.0
profile currently exists. Legacy profiles do not include USGS data newer
than March 11, 2024. 
More details: 
https://doi-usgs.github.io/dataRetrieval/articles/Status.html")
}

wqp_message <- function(){
  message("NEWS: Legacy data profiles will be retired. Please begin converting
workflows to the WQX 3.0 profiles. Also, data from legacy profiles do not
include USGS data newer than March 11, 2024. More details:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html")
}

only_legacy <- function(service){
  legacy <- service %in% c("Organization",
                 "ActivityMetric", "SiteSummary",
                 "Project", "ProjectMonitoringLocationWeighting",
                 "ResultDetectionQuantitationLimit", "BiologicalMetric")
  return(legacy)
}

is_legacy <- function(service){
  legacy <- service %in% c("Result", "Station",
                           "Activity", "Organization",
                           "ActivityMetric", "SiteSummary",
                           "Project", "ProjectMonitoringLocationWeighting",
                           "ResultDetectionQuantitationLimit", "BiologicalMetric")
  return(legacy)
}

wqp_message_now <- function(service){
  if(only_legacy(service)){
    return(wqp_message_only_legacy())
  } else if (service %in% c("Result", "Station", "Activity")){
    return(wqp_message())
  }
}

nwis_message <- function(){
  return("WARNING: NWIS does not deliver
discrete water quality data newer than March 11, 2024
or updates to existing data. For additional details, see:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html")
}
