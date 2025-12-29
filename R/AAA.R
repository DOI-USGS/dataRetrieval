pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {
  suppressMessages(setAccess("public"))
  pkg.env$nldi_base <- "https://api.water.usgs.gov/nldi/linked-data/"
  pkg.env$local_sf <- requireNamespace("sf", quietly = TRUE)
  options("dataRetrieval" = list("api_version" = "v0"))
  
  services <- c("server", "daily", "time-series-metadata",
               "monitoring-locations", "latest-continuous",
               "field-measurements", "latest-daily",
               "continuous")
  collections <- c("parameter-codes", "agency-codes", "altitude-datums", "aquifer-codes",
                   "aquifer-types", "coordinate-accuracy-codes", "coordinate-datum-codes",
                   "coordinate-method-codes", "medium-codes", "counties",
                   "hydrologic-unit-codes", "states",
                   "national-aquifer-codes", "reliability-codes", "site-types", "statistic-codes",
                   "topographic-codes", "time-zone-codes")

  pkg.env$api_endpoints <- services
  pkg.env$metadata <- collections
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

wqp_message <- function(){
  message("NEWS: Data does not include USGS data newer than March 11, 2024. More details:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html")
}

wqp_message_beta <- function(){
  message("WQX3 services are in-development, use with caution.")
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

nwis_message <- function(){
  return("WARNING: whatNWISdata does not include
discrete water quality data newer than March 11, 2024.
For additional details, see:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html")
}

new_nwis_message <- function(){
  return("ALERT: All NWIS services are slated for decommission
and new dataRetrieval functions will be added.
For up-to-date information, see: 
https://doi-usgs.github.io/dataRetrieval/articles/Status.html")
}

