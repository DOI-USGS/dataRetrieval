#' Site Data Import from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function
#' gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on
#' \url{https://www.waterqualitydata.us/webservices_documentation}. The return from
#' this function returns the basic metadata on WQP sites. It is
#' generally faster than the \code{\link{whatWQPdata}} function, but does
#' not return information on what data was collected at the site.
#'
#' The \code{readWQPsummary} function has
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation}
#' for a complete list of options. A list of arguments can also be supplied.
#' One way to figure out how to construct a WQP query is to go to the "Advanced" 
#' form in the Water Quality Portal:
#' \url{https://www.waterqualitydata.us/#mimeType=csv&providers=NWIS&providers=STORET}
#' Use the form to discover what parameters are available. Once the query is 
#' set in the form, scroll down to the "Query URL". You will see the parameters
#' after "https://www.waterqualitydata.us/#". For example, if you chose "Nutrient"
#' in the Characteristic Group dropdown, you will see characteristicType=Nutrient
#' in the Query URL. The corresponding argument for dataRetrieval is
#' characteristicType = "Nutrient". dataRetrieval users do not need to include
#' mimeType, zip, and providers is optional (these arguments are picked automatically).
#' @param legacy Logical. If TRUE, use legacy WQP services. Default is FALSE.
#' @keywords data import WQP web service
#' @rdname wqpSpecials
#' @name whatWQPsites
#' @seealso whatWQPdata readWQPsummary
#' @return data frame 
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
whatWQPsites <- function(..., legacy = FALSE) {
  values <- readWQPdots(..., legacy = legacy)

  values <- values$values

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }

  values <- sapply(values, function(x) utils::URLencode(x, reserved = TRUE))
  
  if(legacy){
    baseURL <- drURL("Station", arg.list = values)
  } else {
    baseURL <- drURL("StationWQX", arg.list = values)
  }
  

  baseURL <- appendDrURL(baseURL, mimeType = "csv")

  retval <- importWQP(baseURL)

  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- baseURL

  return(retval)
}


#' Summary of Data Available from Water Quality Portal
#'
#' Returns a list of sites with year-by-year information on what data is available.
#' The function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on
#' \url{https://www.waterqualitydata.us/webservices_documentation}.
#' The information returned from this function describes the
#' available data at the WQP sites, and some metadata on the sites themselves.
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation}
#'  for a complete list of options. A list of arguments can also be supplied.
#' One way to figure out how to construct a WQP query is to go to the "Advanced" 
#' form in the Water Quality Portal:
#' \url{https://www.waterqualitydata.us/#mimeType=csv&providers=NWIS&providers=STORET}
#' Use the form to discover what parameters are available. Once the query is 
#' set in the form, scroll down to the "Query URL". You will see the parameters
#' after "https://www.waterqualitydata.us/#". For example, if you chose "Nutrient"
#' in the Characteristic Group dropdown, you will see characteristicType=Nutrient
#' in the Query URL. The corresponding argument for dataRetrieval is
#' characteristicType = "Nutrient". dataRetrieval users do not need to include
#' mimeType, and providers is optional (these arguments are picked automatically).
#' @return A data frame from the data returned from the Water Quality Portal
#' @export
#' @seealso whatWQPsites whatWQPdata
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
  
  wqp_message()
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

  values <- sapply(values, function(x) utils::URLencode(x, reserved = TRUE))

  baseURL <- drURL("SiteSummary", arg.list = values)
  baseURL <- appendDrURL(baseURL, mimeType = "csv")

  withCallingHandlers(
    {
      retval <- importWQP(baseURL, 
                          csv = TRUE)
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
