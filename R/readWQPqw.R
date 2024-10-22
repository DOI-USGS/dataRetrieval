#' Raw Data Import for Water Quality Portal
#'
#' Imports data from the Water Quality Portal.
#' This function gets the data from here: \url{https://www.waterqualitydata.us}. There
#' are four required input arguments: siteNumbers, parameterCd, startDate, and endDate.
#' parameterCd can either be a USGS 5-digit code, or a characteristic name. The sites can be
#' either USGS, or other Water Quality Portal offered sites. It is required to use the 'full'
#' site name, such as 'USGS-01234567'.
#'
#' @param siteNumbers character site number. This needs to include the full agency code prefix.
#' @param parameterCd vector of USGS 5-digit parameter code or characteristicNames.
#' Leaving this blank will return all of the measured values during the specified time period.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' Default is "" which indicates
#' retrieval for the earliest possible record. Date arguments are always specified in local time.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' Default is "" which indicates
#' retrieval for the latest possible record. Date arguments are always specified in local time.
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the
#' data provided tz_cd column.
#' Possible values to provide are "America/New_York","America/Chicago",
#' "America/Denver","America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings
#' time: "America/Honolulu",
#' "America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla".
#' See also  \code{OlsonNames()}
#' for more information on time zones.
#' @param querySummary logical to look at number of records and unique sites that
#' will be returned from this query.
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @param ignore_attributes logical to choose to ignore fetching site and parameter
#' attributes. Default is \code{FALSE}.
#' @param legacy Logical. If TRUE, use legacy WQP services. Default is TRUE.
#' Setting legacy = FALSE uses WQX3 services, which are considered in-development, use with caution.
#' @keywords data import USGS web service
#' @return A data frame derived from the default data profile.
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
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}},
#' \code{\link{readNWISqw}}, and \code{\link{importWQP}}
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' rawPcode <- readWQPqw("USGS-01594440", "01075", "", "")
#' 
#' attr(rawPcode, "siteInfo")
#' attr(rawPcode, "queryTime")
#' 
#' 
#' rawCharacteristicName <- readWQPqw("WIDNR_WQX-10032762", "Specific conductance", "", "")
#' rawPHsites <- readWQPqw(c("USGS-05406450", "USGS-05427949", "WIDNR_WQX-133040"), "pH", "", "")
#' nwisEx <- readWQPqw("USGS-04024000", c("34247", "30234", "32104", "34220"), "", "2022-12-20")
#'
#' SC <- readWQPqw(siteNumbers = "USGS-05288705", parameterCd = "00300", convertType = FALSE)
#' }
readWQPqw <- function(siteNumbers,
                      parameterCd,
                      startDate = "",
                      endDate = "",
                      tz = "UTC",
                      legacy = TRUE,
                      querySummary = FALSE,
                      ignore_attributes = FALSE,
                      convertType = TRUE) {
  
  url <- constructWQPURL(siteNumbers, parameterCd, startDate, endDate, legacy)
  
  if (querySummary) {
    retquery <- getQuerySummary(url)
    return(retquery)
  } else {
    retval <- importWQP(url, tz = tz, 
                        convertType = convertType)
    if(is.null(retval)){
      return(NULL)
    }
    attr(retval, "legacy") <- legacy

    if(legacy){
      sites <- unique(retval$MonitoringLocationIdentifier)
    } else {
      sites <- unique(retval$Location_Identifier)
    }
    
    if (!all(is.na(retval)) && !ignore_attributes) {
      retval <- create_WQP_attributes(retval, siteid = sites)
    } 

    if(legacy){
      wqp_message()
    } else {
      wqp_message_beta
    }
    attr(retval, "url") <- url

    return(retval)
  }
}
