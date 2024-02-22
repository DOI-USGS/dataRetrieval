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
#' rawCharacteristicName <- readWQPqw("WIDNR_WQX-10032762", "Specific conductance", "", "")
#' rawPHsites <- readWQPqw(c("USGS-05406450", "USGS-05427949", "WIDNR_WQX-133040"), "pH", "", "")
#' nwisEx <- readWQPqw("USGS-04024000", c("34247", "30234", "32104", "34220"), "", "2012-12-20")
#' nwisEx.summary <- readWQPqw("USGS-04024000", c("34247", "30234", "32104", "34220"),
#'   "", "2012-12-20",
#'   querySummary = TRUE
#' )
#'
#' SC <- readWQPqw(siteNumbers = "USGS-05288705", parameterCd = "00300", convertType = FALSE)
#' }
readWQPqw <- function(siteNumbers,
                      parameterCd,
                      startDate = "",
                      endDate = "",
                      tz = "UTC",
                      querySummary = FALSE,
                      convertType = TRUE) {
  url <- constructWQPURL(siteNumbers, parameterCd, startDate, endDate)
  wqp_message()
  
  if (querySummary) {
    retquery <- getQuerySummary(url)
    return(retquery)
  } else {
    retval <- importWQP(url, zip = TRUE, tz = tz, convertType = convertType)

    pcodeCheck <- all(nchar(parameterCd) == 5) &
      all(!is.na(suppressWarnings(as.numeric(parameterCd))))

    if (nzchar(startDate)) {
      startDate <- format(as.Date(startDate), format = "%m-%d-%Y")
    }

    if (nzchar(endDate)) {
      endDate <- format(as.Date(endDate), format = "%m-%d-%Y")
    }
    
    sites <- unique(retval$MonitoringLocationIdentifier)
    
    siteInfo <- suppressWarnings(whatWQPsites(siteid = paste0(sites, collapse = ";")))

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


    variableInfo <- data.frame(
      characteristicName = retval$CharacteristicName,
      parameterCd = retval$USGSPCode,
      param_units = retval$ResultMeasure.MeasureUnitCode,
      valueType = retval$ResultSampleFractionText,
      stringsAsFactors = FALSE
    )
    variableInfo <- unique(variableInfo)

    if (!anyNA(variableInfo$parameterCd)) {
      pcodes <- unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)])
      pcodes <- pcodes["" != pcodes]
      paramINFO <- readNWISpCode(pcodes)
      names(paramINFO)["parameter_cd" == names(paramINFO)] <- "parameterCd"

      pCodeToName <- pCodeToName
      varExtras <- pCodeToName[pCodeToName$parm_cd %in%
        unique(variableInfo$parameterCd[!is.na(variableInfo$parameterCd)]), ]
      names(varExtras)[names(varExtras) == "parm_cd"] <- "parameterCd"
      variableInfo <- merge(variableInfo, varExtras, by = "parameterCd", all = TRUE)
      variableInfo <- merge(variableInfo, paramINFO, by = "parameterCd", all = TRUE)
      variableInfo <- unique(variableInfo)
    }

    attr(retval, "siteInfo") <- siteInfo
    attr(retval, "variableInfo") <- variableInfo
    attr(retval, "url") <- url
    attr(retval, "queryTime") <- Sys.time()

    return(retval)
  }
}
