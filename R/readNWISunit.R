#' Instantaneous value data retrieval from USGS (NWIS)
#'
#' Imports data from NWIS web service.
#' Inputs to this function are USGS site ids, USGS parameter codes,
#' and start and end date. For a more complex query, use [readNWISdata()],
#' including an arguement service="uv".
#' Not all parameter codes are available for all data.
#' Use the function [whatNWISdata()] to discover what data
#' is available for a USGS site. The column data_type_cd with the values "uv"
#' returned from [whatNWISdata()]) are available from this service.
#'
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record. Simple date arguments are specified in local time.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record. Simple date arguments are specified in local time.
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the data provided tz_cd column.
#' Possible values to provide are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla". See also  `OlsonNames()`
#' for more information on time zones.
#' @param override_error Logical parameter that allows users to run this
#' function during the final stages of NWIS decommission. NWIS servers will be taken
#' offline February 22nd, 2027. Setting this parameter to `TRUE` should only
#' be used as a tool to convert readNWIS to read_waterdata functions.
#' @keywords data import USGS web service
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' dateTime \tab POSIXct \tab The date and time of the value converted to UTC \cr
#' tz_cd \tab character \tab The time zone code for dateTime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form:
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
#' @seealso [renameNWISColumns()], [importWaterML1()]
#' @export
#' @examplesIf is_dataRetrieval_user()
#' site_id <- "05114000"
#' parameterCd <- "00060"
#' startDate <- "2014-10-10"
#' endDate <- "2014-10-10"
#' \donttest{
#'
#' #rawData <- readNWISuv(site_id, parameterCd, startDate, endDate)
#'
#' #rawData_today <- readNWISuv(site_id, parameterCd, Sys.Date(), Sys.Date())
#'
#' }
readNWISuv <- function(
  siteNumbers,
  parameterCd,
  startDate = "",
  endDate = "",
  tz = "UTC",
  override_error = FALSE
) {
  if (override_error) {
    warning(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISuv to read_waterdata_continuous."
    )

    if (
      as.character(startDate) == "" || (as.Date(startDate) <= Sys.Date() - 120)
    ) {
      service <- "iv"
    } else {
      service <- "iv_recent"
    }

    url <- constructNWISURL(
      siteNumbers,
      parameterCd,
      startDate,
      endDate,
      service,
      format = "xml"
    )

    data <- importWaterML1(url, asDateTime = TRUE, tz = tz)

    return(data)
  } else {
    stop(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISuv to read_waterdata_continuous.
Use override_error = TRUE to temporarily override this error. For more information:
https://doi-usgs.github.io/dataRetrieval/articles/read_waterdata_functions.html"
    )
  }
}

#' Peak flow data from USGS (NWIS)
#'
#' Reads peak flow from NWISweb.
#'
#' In some cases, the specific date of the peak data is not know. This function
#' will default to
#' converting complete dates to a "Date" object, and converting incomplete dates to
#' "NA". If those incomplete dates are
#' needed, set the `asDateTime` argument to FALSE. No dates will be converted to
#' R Date objects.
#'
#' @param siteNumbers character USGS site number(or multiple sites).  This is usually
#' an 8 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' Default is "" which indicates
#' retrieval for the latest possible record.
#' @param asDateTime logical default to `TRUE`. When `TRUE`, the peak_dt column is converted
#' to a Date object, and incomplete dates are removed. When `FALSE`, no
#' columns are removed, but no dates are converted.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @param override_error Logical parameter that allows users to run this
#' function during the final stages of NWIS decommission. NWIS servers will be taken
#' offline February 22nd, 2027. Setting this parameter to `TRUE` should only
#' be used as a tool to convert readNWIS to read_waterdata functions.
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' peak_dt \tab Date \tab Date of peak streamflow \cr
#' peak_tm \tab character \tab Time of peak streamflow as character \cr
#' peak_va \tab numeric \tab Annual peak streamflow value in cfs \cr
#' peak_cd \tab character \tab Peak Discharge-Qualification codes (see `comment`
#' for more information) \cr
#' gage_ht \tab numeric \tab Gage height for the associated peak streamflow in feet \cr
#' gage_ht_cd \tab character \tab Gage height qualification codes \cr
#' year_last_pk \tab numeric \tab Peak streamflow reported is the highest since this year \cr
#' ag_dt \tab Date \tab Date of maximum gage-height for water year
#' (if not concurrent with peak) \cr
#' ag_tm \tab character \tab Time of maximum gage-height for water year
#' (if not concurrent with peak) \cr
#' ag_gage_ht \tab numeric \tab maximum Gage height for water year in feet
#' (if not concurrent with peak) \cr
#' ag_gage_ht_cd \tab character \tab maximum Gage height code \cr
#' }
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' }
#' @seealso [constructNWISURL()], [importRDB1()]
#' @export
#' @examplesIf is_dataRetrieval_user()
#' #site_ids <- c("01594440", "040851325")
#' \donttest{
#' #data <- readNWISpeak(site_ids)
#' #data2 <- readNWISpeak(site_ids, asDateTime = FALSE)
#' #stations <- c("06011000")
#' #peakdata <- readNWISpeak(stations, convertType = FALSE)
#' }
readNWISpeak <- function(
  siteNumbers,
  startDate = "",
  endDate = "",
  asDateTime = TRUE,
  convertType = TRUE,
  override_error = FALSE
) {
  if (override_error) {
    warning(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISpeak to read_waterdata_peaks."
    )
    # Doesn't seem to be a peak xml service
    url <- constructNWISURL(
      siteNumbers = siteNumbers,
      parameterCd = NA,
      startDate = startDate,
      endDate = endDate,
      service = "peak"
    )

    data <- importRDB1(url, asDateTime = asDateTime, convertType = convertType)

    if (nrow(data) > 0) {
      if (asDateTime && convertType) {
        if ("peak_dt" %in% names(data)) {
          if (
            any(nchar(as.character(data$peak_dt)) <= 7, na.rm = TRUE) ||
              any(grepl("[0-9]*-[0-9]*-00", data$peak_dt), na.rm = TRUE)
          ) {
            stop(
              "Not all dates could be converted to Date object. Use convertType=FALSE to retrieve the raw text"
            )
          } else {
            data$peak_dt <- as.Date(data$peak_dt, format = "%Y-%m-%d")
          }
          if (anyNA(data$peak_dt)) {
            message(
              "Some dates could not be converted to a valid date, and were returned as NA"
            )
          }
        }

        if ("ag_dt" %in% names(data)) {
          data$ag_dt <- as.Date(data$ag_dt, format = "%Y-%m-%d")
        }
      }

      siteInfo <- suppressWarnings(readNWISsite(
        siteNumbers,
        override_error = TRUE
      ))
      siteInfo <- merge(
        x = unique(data[, c("agency_cd", "site_no")]),
        y = siteInfo,
        by = c("agency_cd", "site_no"),
        all.x = TRUE
      )

      attr(data, "siteInfo") <- siteInfo
      attr(data, "variableInfo") <- NULL
      attr(data, "statisticInfo") <- NULL
    }

    return(data)
  } else {
    stop(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISdv to read_waterdata_peaks.
Use override_error = TRUE to temporarily override this error. For more information:
https://doi-usgs.github.io/dataRetrieval/articles/read_waterdata_functions.html"
    )
  }
}

#' Rating table for an active USGS streamgage retrieval
#'
#' Reads current rating table for an active USGS streamgage from NWISweb.
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param type character can be "base", "corr", or "exsa"
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @param override_error Logical parameter that allows users to run this
#' function during the final stages of NWIS decommission. NWIS servers will be taken
#' offline February 22nd, 2027. Setting this parameter to `TRUE` should only
#' be used as a tool to convert readNWIS to read_waterdata functions.
#' @return A data frame. If `type` is "base, " then the columns are
#' INDEP, typically the gage height, in feet; DEP, typically the streamflow,
#' in cubic feet per second; and STOR, where "*" indicates that the pair are
#' a fixed point of the rating curve. If `type` is "exsa, " then an
#' additional column, SHIFT, is included that indicates the current shift in
#' the rating for that value of INDEP. If `type` is "corr, " then the
#' columns are INDEP, typically the gage height, in feet; CORR, the correction
#' for that value; and CORRINDEP, the corrected value for CORR.\cr
#' If `type` is "base, " then the data frame has an attribute called "RATING"
#' that describes the rating curve is included.
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' RATING \tab character \tab Rating information \cr
#' }
#'
#' @note Not all active USGS streamgages have traditional rating curves that
#' relate flow to stage.
#' @seealso [constructNWISURL()], [importRDB1()]
#' @export
#' @examplesIf is_dataRetrieval_user()
#' site_id <- "01594440"
#' \donttest{
#' #data <- readNWISrating(site_id, "base")
#' #attr(data, "RATING")
#' }
readNWISrating <- function(
  siteNumber,
  type = "base",
  convertType = TRUE,
  override_error = FALSE
) {
  if (override_error) {
    warning(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISrating to read_waterdata_ratings."
    )

    # No rating xml service
    url <- constructNWISURL(siteNumber, service = "rating", ratingType = type)

    data <- importRDB1(url, asDateTime = FALSE, convertType = convertType)

    if ("current_rating_nu" %in% names(data)) {
      data$current_rating_nu <- gsub(" ", "", data$current_rating_nu)
    }

    if (nrow(data) > 0) {
      if (type == "base") {
        Rat <- grep("//RATING ", comment(data), value = TRUE, fixed = TRUE)
        Rat <- sub("# //RATING ", "", Rat)
        Rat <- scan(text = Rat, sep = " ", what = "")
        attr(data, "RATING") <- Rat
      }

      siteInfo <- suppressWarnings(readNWISsite(
        siteNumbers = siteNumber,
        override_error = TRUE
      ))

      attr(data, "siteInfo") <- siteInfo
      attr(data, "variableInfo") <- NULL
      attr(data, "statisticInfo") <- NULL
    }

    return(data)
  } else {
    stop(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISrating to read_waterdata_ratings.
Use override_error = TRUE to temporarily override this error. For more information:
https://doi-usgs.github.io/dataRetrieval/articles/read_waterdata_functions.html"
    )
  }
}


#' Site statistics retrieval from USGS (NWIS)
#'
#' Retrieves site statistics from the USGS Statistics Web Service beta.
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number.
#' @param parameterCd character USGS parameter code.  This is usually a 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY, YYYY-MM, or YYYY-MM-DD. Dates cannot
#' be more specific than the statReportType, i.e. startDate for monthly statReportTypes cannot include days, and annual
#' statReportTypes cannot include days or months.  Months and days are optional for the daily statReportType.
#' Default is "" which indicates retrieval for the earliest possible record.  For daily data, this indicates the
#' start of the period the statistics will be computed over.
#' @param endDate character ending date for data retrieval in the form YYYY, YYYY-MM, or YYYY-MM-DD. Default is ""
#' which indicates retrieval for the latest possible record.  For daily data, this
#' indicates the end of the period
#' the statistics will be computed over.  The same restrictions as startDate apply.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function will convert the data to
#' numerics based on a standard algorithm. Years, months, and days (if appliccable) are also returned as numerics
#' in separate columns.  If convertType is false, everything is returned as a character.
#' @param statReportType character time division for statistics: daily, monthly, or annual.  Default is daily.
#' Note that daily provides statistics for each calendar day over the specified range
#' of water years, i.e. no more than 366
#' data points will be returned for each site/parameter.  Use readNWISdata or readNWISdv for daily averages.
#' Also note that 'annual' returns statistics for the calendar year.  Use readNWISdata
#' for water years. Monthly and yearly
#' provide statistics for each month and year within the range indivually.
#' @param statType character type(s) of statistics to output for daily values.
#' Default is mean, which is the only option for monthly and yearly report types.
#' @param override_error Logical parameter that allows users to run this
#' function during the final stages of NWIS decommission. NWIS servers will be taken
#' offline February 22nd, 2027. Setting this parameter to `TRUE` should only
#' be used as a tool to convert readNWIS to read_waterdata functions.
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' parameter_cd \tab character \tab The USGS parameter code \cr
#'
#' Other columns will be present depending on statReportType and statType
#' }
#' @seealso [constructNWISURL()], [importRDB1()]
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' # x1 <- readNWISstat(
#' #   siteNumbers = c("02319394"),
#' #   parameterCd = c("00060"),
#' #   statReportType = "annual"
#' # )
#'
#' # all the annual mean discharge data for two sites
#' #x2 <- readNWISstat(
#' #   siteNumbers = c("02319394", "02171500"),
#' #   parameterCd = c("00010", "00060"),
#' #   statReportType = "annual"
#' # )
#'
#' # Request p25, p75, and mean values for temperature and discharge for the 2000s
#' # Note that p25 and p75 were not available for temperature, and return NAs
#' #x <- readNWISstat(
#' #   siteNumbers = c("02171500"),
#' #   parameterCd = c("00010", "00060"),
#' #   statReportType = "daily",
#' #   statType = c("mean", "median"),
#' #   startDate = "2000", endDate = "2010"
#' # )
#' }
readNWISstat <- function(
  siteNumbers,
  parameterCd,
  startDate = "",
  endDate = "",
  convertType = TRUE,
  statReportType = "daily",
  statType = "mean",
  override_error = FALSE
) {
  if (override_error) {
    warning(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISstat to read_waterdata_stats_por"
    )
    # check for NAs in site numbers
    if (any(is.na(siteNumbers))) {
      siteNumbers <- siteNumbers[!is.na(siteNumbers)]
      if (length(siteNumbers) == 0) {
        stop("siteNumbers was all NAs")
      }
      warning("NAs were passed in siteNumbers; they were ignored")
    }
    url <- constructNWISURL(
      siteNumbers = siteNumbers,
      parameterCd = parameterCd,
      startDate = startDate,
      endDate = endDate,
      service = "stat",
      format = "rdb",
      statType = statType,
      statReportType = statReportType
    )

    data <- importRDB1(
      obs_url = url,
      asDateTime = TRUE,
      convertType = convertType
    )

    siteInfo <- suppressWarnings(readNWISsite(
      siteNumbers,
      override_error = TRUE
    ))

    if (nrow(data) > 0) {
      siteInfo <- merge(
        x = unique(data[, c("agency_cd", "site_no")]),
        y = siteInfo,
        by = c("agency_cd", "site_no"),
        all.x = TRUE
      )
    }

    attr(data, "siteInfo") <- siteInfo

    return(data)
  } else {
    stop(
      "NWIS servers will be taken offline February 22nd, 2027. Update readNWISstat to read_waterdata_stats_por
Use override_error = TRUE to temporarily override this error. For more information:
https://doi-usgs.github.io/dataRetrieval/articles/read_waterdata_functions.html"
    )
  }
}
