pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {
  suppressMessages(setAccess("public"))
  pkg.env$nldi_base <- "https://api.water.usgs.gov/nldi/linked-data/"
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
  return("WARNING: NWIS does not deliver
discrete water quality data newer than March 11, 2024
or updates to existing data. For additional details, see:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html")
}
