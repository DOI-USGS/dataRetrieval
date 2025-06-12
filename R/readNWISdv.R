#' Daily Value USGS NWIS Data Retrieval
#'
#' Imports data from NWIS daily web service. This function gets the data from here: 
#' <https://waterservices.usgs.gov/docs/dv-service/daily-values-service-details/>
#' Inputs to this function are just USGS site ids, USGS parameter codes,
#' USGS statistic codes, and start and end date. For a more complex query, use [readNWISdata()],
#' with an argument service = "dv".
#' Data coming the daily web services are aggregates of the instantaneous
#' (sensor) web services. Not all statistical codes are available for all data.
#' Use the function [whatNWISdata()] to discover what data
#' is available for a USGS site. The column data_type_cd with the values "dv"
#' returned from [whatNWISdata()]) are available from this service.
#' 
#' More information on the web service can be found here:
#' <https://waterservices.usgs.gov/test-tools>, choosing the
#' "Daily Value Service".
#'
#' @param siteNumbers character USGS site number.  This is usually an 8 digit number.
#' Multiple sites can be requested with a character vector.
#' @param parameterCd character of USGS parameter code(s).  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' Default is "" which indicates
#' retrieval for the earliest possible record. Date arguments are always specified in local time.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record. Date arguments are always specified in local time.
#' @param statCd character USGS statistic code. This is usually 5 digits.  Daily mean (00003) is the default.
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency \tab character \tab The NWIS code for the agency reporting the data\cr
#' site \tab character \tab The USGS site number \cr
#' Date \tab Date \tab The date of the value \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form
#' X_D_P_S, where X is literal,
#' D is an option description of the parameter,
#' P is the parameter code,
#' and S is the statistic code (if applicable).
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' variableInfo \tab data.frame \tab A data frame containing information on the requested parameters \cr
#' statisticInfo \tab data.frame \tab A data frame containing information on the requested statistics on the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#'
#' @seealso [read_USGS_daily()]
#' @export
#' @keywords data import USGS web service
#' @examples
#' 
#' # see ?read_USGS_daily
#' 
#' #site_id <- "04085427"
#' #startDate <- "2012-01-01"
#' #endDate <- "2012-06-30"
#' #pCode <- "00060"
#' #
#' #rawDailyQ <- readNWISdv(site_id, pCode, startDate, endDate)
#' 
readNWISdv <- function(siteNumbers,
                       parameterCd,
                       startDate = "",
                       endDate = "",
                       statCd = "00003") {
  
  .Deprecated(new = "read_USGS_daily",
              package = "dataRetrieval", 
              msg = "NWIS servers are slated for decommission. Please begin to migrate to read_USGS_daily.")
  
  url <- constructNWISURL(
    siteNumbers = siteNumbers,
    parameterCd = parameterCd,
    startDate = startDate,
    endDate = endDate,
    service = "dv",
    statCd = statCd
  )

  data <- importWaterML1(url, asDateTime = FALSE)

  if (nrow(data) > 0) {
    data$dateTime <- as.Date(data$dateTime)
    data$tz_cd <- NULL

    names(data)[names(data) == "dateTime"] <- "Date"
  }

  return(data)
}
