#' Instantaneous value data retrieval from USGS (NWIS)
#'
#' Imports data from NWIS web service. This function gets the data from here:
#' \url{https://waterservices.usgs.gov/}
#' A list of parameter codes can be found here:
#' \url{https://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' A list of statistic codes can be found here:
#' \url{https://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}.
#' More information on the web service can be found here:
#' \url{https://waterservices.usgs.gov/docs/instantaneous-values/}.
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record. Simple date arguments are specified in local time.
#' See more information here: \url{https://waterservices.usgs.gov/docs/instantaneous-values/}.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record. Simple date arguments are specified in local time.
#' See more information here: \url{https://waterservices.usgs.gov/docs/instantaneous-values/}.
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the data's provided tz_cd column.
#' Possible values to provide are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()}
#' for more information on time zones.
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
#' @seealso \code{\link{renameNWISColumns}}, \code{\link{importWaterML1}}
#' @export
#' @examplesIf is_dataRetrieval_user()
#' site_id <- "05114000"
#' parameterCd <- "00060"
#' startDate <- "2014-10-10"
#' endDate <- "2014-10-10"
#' \donttest{
#'
#' rawData <- readNWISuv(site_id, parameterCd, startDate, endDate)
#'
#' rawData_today <- readNWISuv(site_id, parameterCd, Sys.Date(), Sys.Date())
#'
#' timeZoneChange <- readNWISuv(
#'   c("04024430", "04024000"), parameterCd,
#'   "2013-11-03", "2013-11-03"
#' )
#'
#' centralTime <- readNWISuv(site_id, parameterCd,
#'   "2014-10-10T12:00", "2014-10-10T23:59",
#'   tz = "America/Chicago"
#' )
#'
#' # Adding 'Z' to the time indicates to the web service to call the data with UTC time:
#' GMTdata <- readNWISuv(
#'   site_id, parameterCd,
#'   "2014-10-10T00:00Z", "2014-10-10T23:59Z"
#' )
#' }
readNWISuv <- function(siteNumbers, parameterCd, startDate = "", endDate = "", tz = "UTC") {
  if (as.character(startDate) == "" || (as.Date(startDate) <= Sys.Date() - 120)) {
    service <- "iv"
  } else {
    service <- "iv_recent"
  }

  url <- constructNWISURL(siteNumbers,
    parameterCd,
    startDate,
    endDate,
    service,
    format = "xml"
  )

  data <- importWaterML1(url, asDateTime = TRUE, tz = tz)

  return(data)
}

#' Peak flow data from USGS (NWIS)
#'
#' Reads peak flow from NWISweb. Data is retrieved from
#' \url{https://waterdata.usgs.gov/nwis}.
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
#' @param asDateTime logical default to \code{TRUE}. When \code{TRUE}, the peak_dt column is converted
#' to a Date object, and incomplete dates are removed. When \code{FALSE}, no
#' columns are removed, but no dates are converted.
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' peak_dt \tab Date \tab Date of peak streamflow \cr
#' peak_tm \tab character \tab Time of peak streamflow as character \cr
#' peak_va \tab numeric \tab Annual peak streamflow value in cfs \cr
#' peak_cd \tab character \tab Peak Discharge-Qualification codes (see \code{comment}
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
#' @seealso \code{\link{constructNWISURL}}, \code{\link{importRDB1}}
#' @export
#' @examplesIf is_dataRetrieval_user()
#' site_ids <- c("01594440", "040851325")
#' \donttest{
#' data <- readNWISpeak(site_ids)
#' data2 <- readNWISpeak(site_ids, asDateTime = FALSE)
#' stations <- c("06011000")
#' peakdata <- readNWISpeak(stations, convertType = FALSE)
#' }
readNWISpeak <- function(siteNumbers,
                         startDate = "",
                         endDate = "",
                         asDateTime = TRUE,
                         convertType = TRUE) {

  # Doesn't seem to be a peak xml service
  url <- constructNWISURL(
    siteNumbers = siteNumbers,
    parameterCd = NA,
    startDate = startDate,
    endDate = endDate,
    service = "peak"
  )

  data <- importRDB1(url,
    asDateTime = asDateTime,
    convertType = convertType
  )

  if (nrow(data) > 0) {
    if (asDateTime && convertType) {
      if ("peak_dt" %in% names(data)) {
        if (any(nchar(as.character(data$peak_dt)) <= 7, na.rm = TRUE) ||
          any(grepl("[0-9]*-[0-9]*-00", data$peak_dt), na.rm = TRUE)) {
          stop("Not all dates could be converted to Date object. Use convertType=FALSE to retrieve the raw text")
        } else {
          data$peak_dt <- as.Date(data$peak_dt, format = "%Y-%m-%d")
        }
        if (anyNA(data$peak_dt)) {
          message("Some dates could not be converted to a valid date, and were returned as NA")
        }
      }

      if ("ag_dt" %in% names(data)) data$ag_dt <- as.Date(data$ag_dt, format = "%Y-%m-%d")
    }


    siteInfo <- readNWISsite(siteNumbers)
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
}

#' Rating table for an active USGS streamgage retrieval
#'
#' Reads current rating table for an active USGS streamgage from NWISweb.
#' Data is retrieved from \url{https://waterdata.usgs.gov/nwis}.
#'
#' @param siteNumber character USGS site number.  This is usually an 8 digit number
#' @param type character can be "base", "corr", or "exsa"
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @return A data frame. If \code{type} is "base, " then the columns are
#' INDEP, typically the gage height, in feet; DEP, typically the streamflow,
#' in cubic feet per second; and STOR, where "*" indicates that the pair are
#' a fixed point of the rating curve. If \code{type} is "exsa, " then an
#' additional column, SHIFT, is included that indicates the current shift in
#' the rating for that value of INDEP. If \code{type} is "corr, " then the
#' columns are INDEP, typically the gage height, in feet; CORR, the correction
#' for that value; and CORRINDEP, the corrected value for CORR.\cr
#' If \code{type} is "base, " then the data frame has an attribute called "RATING"
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
#' @seealso \code{\link{constructNWISURL}}, \code{\link{importRDB1}}
#' @export
#' @examplesIf is_dataRetrieval_user()
#' site_id <- "01594440"
#' \donttest{
#' data <- readNWISrating(site_id, "base")
#' attr(data, "RATING")
#' }
readNWISrating <- function(siteNumber, type = "base", convertType = TRUE) {

  # No rating xml service
  url <- constructNWISURL(siteNumber, service = "rating", ratingType = type)

  data <- importRDB1(url, asDateTime = FALSE, convertType = convertType)

  if ("current_rating_nu" %in% names(data)) {
    intColumns <- intColumns[!("current_rating_nu" %in% names(data)[intColumns])]
    data$current_rating_nu <- gsub(" ", "", data$current_rating_nu)
  }

  if (nrow(data) > 0) {
    if (type == "base") {
      Rat <- grep("//RATING ", comment(data), value = TRUE, fixed = TRUE)
      Rat <- sub("# //RATING ", "", Rat)
      Rat <- scan(text = Rat, sep = " ", what = "")
      attr(data, "RATING") <- Rat
    }

    siteInfo <- readNWISsite(siteNumber)

    attr(data, "siteInfo") <- siteInfo
    attr(data, "variableInfo") <- NULL
    attr(data, "statisticInfo") <- NULL
  }

  return(data)
}

#' Surface-water measurement data retrieval from USGS (NWIS)
#'
#' Reads surface-water measurement data from NWISweb. Data is retrieved from \url{https://waterdata.usgs.gov/nwis}.
#' See \url{https://waterdata.usgs.gov/usa/nwis/sw} for details about surface water.
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the data's provided tz_cd column.
#' Possible values to provide are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()}
#' for more information on time zones.
#' @param expanded logical. Whether or not (TRUE or FALSE) to call the expanded data.
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will
#' convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @return A data frame with at least the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' measurement_dt \tab POSIXct \tab The date and time (in POSIXct) of the measurement. Unless specified
#' with the tz parameter, this is converted to UTC. If the measurement_dt column is
#' an incomplete, a measurement_dt_date and
#' measurement_dt_time column are added to the returned data frame.   \cr
#' tz_cd \tab character \tab The time zone code for the measurement_dt column \cr
#' }
#'
#' See \url{https://waterdata.usgs.gov/usa/nwis/sw} for details about surface water, and
#' \url{https://waterdata.usgs.gov/nwis/help?output_formats_help}
#' for help on the columns and codes.
#'
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' comment \tab character \tab Header comments from the RDB file \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' tz_cd_reported \tab The originally reported time zone \cr
#' }
#' @seealso \code{\link{constructNWISURL}}, \code{\link{importRDB1}}
#' @export
#' @examplesIf is_dataRetrieval_user()
#' site_ids <- c("01594440", "040851325")
#' \donttest{
#' data <- readNWISmeas(site_ids)
#' Meas05316840 <- readNWISmeas("05316840")
#' Meas05316840.ex <- readNWISmeas("05316840", expanded = TRUE)
#' Meas07227500.ex <- readNWISmeas("07227500", expanded = TRUE)
#' Meas07227500.exRaw <- readNWISmeas("07227500", expanded = TRUE, convertType = FALSE)
#' }
readNWISmeas <- function(siteNumbers,
                         startDate = "",
                         endDate = "",
                         tz = "UTC",
                         expanded = FALSE,
                         convertType = TRUE) {

  # Doesn't seem to be a WaterML1 format option
  url <- constructNWISURL(
    siteNumbers = siteNumbers,
    parameterCd = NA,
    startDate = startDate,
    endDate = endDate,
    service = "meas",
    expanded = expanded
  )

  data <- importRDB1(
    obs_url = url,
    asDateTime = TRUE,
    tz = tz,
    convertType = convertType
  )

  if (nrow(data) > 0) {
    if ("diff_from_rating_pc" %in% names(data)) {
      data$diff_from_rating_pc <- as.numeric(data$diff_from_rating_pc)
    }

    url <- attr(data, "url")
    comment <- attr(data, "comment")
    queryTime <- attr(data, "queryTime")
    header <- attr(data, "headerInfo")

    if (convertType) {
      data$measurement_dateTime <- data$measurement_dt
      data$measurement_dt <- suppressWarnings(as.Date(data$measurement_dateTime))
      data$measurement_tm <- strftime(data$measurement_dateTime, "%H:%M")
      data$measurement_tm[is.na(data$tz_cd_reported)] <- ""
      indexDT <- which("measurement_dt" == names(data))
      indexTZ <- which("tz_cd" == names(data))
      indexTM <- which("measurement_tm" == names(data))
      indexTZrep <- which("tz_cd_reported" == names(data))
      newOrder <- c(
        1:indexDT, indexTM, indexTZrep,
        c((indexDT + 1):ncol(data))[!(c((indexDT + 1):ncol(data)) %in%
          c(indexTZrep, indexTM, indexTZ))],
        indexTZ
      )

      data <- data[, newOrder]
    }


    siteInfo <- readNWISsite(siteNumbers)
    siteInfo <- merge(
      x = unique(data[, c("agency_cd", "site_no")]),
      y = siteInfo,
      by = c("agency_cd", "site_no"),
      all.x = TRUE
    )
    attr(data, "url") <- url
    attr(data, "comment") <- comment
    attr(data, "queryTime") <- queryTime
    attr(data, "header") <- header

    attr(data, "siteInfo") <- siteInfo
    attr(data, "variableInfo") <- NULL
    attr(data, "statisticInfo") <- NULL
  }

  return(data)
}

#' Groundwater level measurements retrieval from USGS (NWIS)
#'
#' Reads groundwater level measurements from NWISweb. Mixed date/times come back from the service
#' depending on the year that the data was collected. See \url{https://waterdata.usgs.gov/usa/nwis/gw}
#' for details about groundwater. By default the returned dates are converted to date objects, unless convertType
#' is specified as FALSE. Sites with non-standard date formats (i.e. lacking a day) can be affected (see examples).
#' See \url{https://waterservices.usgs.gov/docs/groundwater-levels/} for more information.
#'
#' @param siteNumbers character USGS site number (or multiple sites).  This is usually an 8 digit number
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the earliest possible record.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is "" which indicates
#' retrieval for the latest possible record.
#' @param parameterCd character USGS parameter code.  This is usually an 5 digit number. Default is "".
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the
#' function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the data's provided tz_cd column.
#' Possible values to provide are "America/New_York", "America/Chicago", "America/Denver", "America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()}
#' for more information on time zones.
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' site_tp_cd \tab character \tab Site type code \cr
#' lev_dt \tab Date \tab Date level measured\cr
#' lev_tm \tab character \tab Time level measured \cr
#' lev_tz_cd \tab character \tab Time datum \cr
#' lev_va \tab numeric \tab Water level value in feet below land surface\cr
#' sl_lev_va \tab numeric \tab Water level value in feet above specific vertical datum \cr
#' lev_status_cd \tab character \tab The status of the site at the time the water level was measured \cr
#' lev_agency_cd \tab character \tab The agency code of the person measuring the water level \cr
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
#'
#' @seealso \code{\link{constructNWISURL}}, \code{\link{importRDB1}}
#' @export
#' @examplesIf is_dataRetrieval_user()
#' site_id <- "434400121275801"
#' \donttest{
#' data <- readNWISgwl(site_id)
#' sites <- c("434400121275801", "375907091432201")
#' data2 <- readNWISgwl(site_id, "", "")
#' data3 <- readNWISgwl("420125073193001", "", "")
#' # handling of data where date has no day
#' data4 <- readNWISgwl("425957088141001", startDate = "1980-01-01")
#'
#' data5 <- readNWISgwl("263819081585801", parameterCd = "72019")
#' }
readNWISgwl <- function(siteNumbers,
                        startDate = "",
                        endDate = "",
                        parameterCd = NA,
                        convertType = TRUE, tz = "UTC") {
  url <- constructNWISURL(
    siteNumbers = siteNumbers,
    parameterCd = parameterCd,
    startDate = startDate,
    endDate = endDate,
    service = "gwlevels",
    format = "rdb"
  )

  data <- importRDB1(
    obs_url = url,
    asDateTime = TRUE,
    convertType = convertType,
    tz = tz
  )

  if (nrow(data) > 0 && !all(is.na(data$lev_dt))) {
    if (convertType) {
      # check that the date includes a day, based on date string length
      if (any(nchar(as.character(data$lev_dt)) <= 7) || any(grepl("[0-9]*-[0-9]*-00", data$lev_dt))) {
        message("Not all dates were converted to Date object. Returning raw text for date columns.")
      } else {
        data$lev_dt <- as.Date(data$lev_dt)
      }
    }
    siteInfo <- readNWISsite(siteNumbers)
    siteInfo <- merge(
      x = unique(data[, c("agency_cd", "site_no")]),
      y = siteInfo,
      by = c("agency_cd", "site_no"),
      all.x = TRUE
    )
    attr(data, "siteInfo") <- siteInfo
  }

  return(data)
}

#' Site statistics retrieval from USGS (NWIS)
#'
#' Retrieves site statistics from the USGS Statistics Web Service beta.
#' See \url{https://waterservices.usgs.gov/docs/statistics/} for more information.
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
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to
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
#' Default is mean, which is the only
#' option for monthly and yearly report types. See the statistics service documentation
#' at \url{https://waterservices.usgs.gov/docs/statistics/} for a full list of codes.
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency_cd \tab character \tab The NWIS code for the agency reporting the data\cr
#' site_no \tab character \tab The USGS site number \cr
#' parameter_cd \tab character \tab The USGS parameter code \cr
#'
#' Other columns will be present depending on statReportType and statType
#' }
#' @seealso \code{\link{constructNWISURL}}, \code{\link{importRDB1}}
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' x1 <- readNWISstat(
#'   siteNumbers = c("02319394"),
#'   parameterCd = c("00060"),
#'   statReportType = "annual"
#' )
#'
#' # all the annual mean discharge data for two sites
#' x2 <- readNWISstat(
#'   siteNumbers = c("02319394", "02171500"),
#'   parameterCd = c("00010", "00060"),
#'   statReportType = "annual"
#' )
#'
#' # Request p25, p75, and mean values for temperature and discharge for the 2000s
#' # Note that p25 and p75 were not available for temperature, and return NAs
#' x <- readNWISstat(
#'   siteNumbers = c("02171500"),
#'   parameterCd = c("00010", "00060"),
#'   statReportType = "daily",
#'   statType = c("mean", "median"),
#'   startDate = "2000", endDate = "2010"
#' )
#' }
readNWISstat <- function(siteNumbers, parameterCd, startDate = "", endDate = "", convertType = TRUE,
                         statReportType = "daily", statType = "mean") {

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

  siteInfo <- readNWISsite(siteNumbers)

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
}

#' Water use data retrieval from USGS (NWIS)
#'
#' Retrieves water use data from USGS Water Use Data for the Nation.  See
#' \url{https://waterdata.usgs.gov/nwis/wu} for
#' more information.  All available use categories for the supplied arguments are retrieved.
#'
#' @param stateCd could be character (full name, abbreviation, id), or numeric (id).
#' Only one is accepted per query.
#' @param countyCd could be character (name, with or without "County", or "ALL"),
#' numeric (id), or code{NULL}, which will
#' return state or national data depending on the stateCd argument.  "ALL" may
#' also be supplied, which will return data
#' for every county in a state. Can be a vector of counties in the same state.
#' @param years integer Years for data retrieval. Must be years ending in 0 or 5.
#' Default is all available years.
#' @param categories character categories of water use.  Defaults to "ALL".
#' Specific categories must be supplied as two-
#' letter abbreviations as seen in the URL when using the NWIS water use web interface.  Note that
#' there are different codes for national and state level data.
#' @param convertType logical defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to
#' numerics based on a standard algorithm. Years, months, and days (if appliccable) are
#' also returned as numerics
#' in separate columns.  If convertType is false, everything is returned as a character.
#' @param transform logical only intended for use with national data.  Defaults to
#' \code{FALSE}, with data being returned as
#' presented by the web service.  If \code{TRUE}, data will be transformed and
#' returned with column names, which will reformat
#' national data to be similar to state data.
#' @return A data frame with at least the year of record, and all available
#' statistics for the given geographic parameters.
#' County and state fields will be included as appropriate.
#'
#' @export
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' # All data for a county
#' allegheny <- readNWISuse(stateCd = "Pennsylvania", countyCd = "Allegheny")
#'
#' # Data for an entire state for certain years
#' ohio <- readNWISuse(years = c(2000, 2005, 2010), stateCd = "OH", countyCd = NULL)
#'
#' # Data for an entire state, county by county
#' pr <- readNWISuse(years = c(2000, 2005, 2010), stateCd = "PR", countyCd = "ALL")
#'
#' # All national-scale data, transforming data frame to named columns from named rows
#' national <- readNWISuse(stateCd = NULL, countyCd = NULL, transform = TRUE)
#'
#' # Washington, DC data
#' dc <- readNWISuse(stateCd = "DC", countyCd = NULL)
#'
#' # data for multiple counties, with different input formatting
#' paData <- readNWISuse(stateCd = "42", countyCd = c("Allegheny County", "BUTLER", 1, "031"))
#'
#' # retrieving two specific categories for an entire state
#' ks <- readNWISuse(stateCd = "KS", countyCd = NULL, categories = c("IT", "LI"))
#' }
readNWISuse <- function(stateCd,
                        countyCd,
                        years = "ALL",
                        categories = "ALL",
                        convertType = TRUE,
                        transform = FALSE) {
  countyID <- NULL
  countyCd <- countyCd[countyCd != ""]

  if (exists("countyCd") && !is.null(countyCd)) {
    if (!any(toupper(countyCd) == "ALL")) {
      for (c in countyCd) {
        code <- countyCdLookup(state = stateCd, county = c, outputType = "id")
        countyID <- c(countyID, code)
      }
    } else {
      countyID <- toupper(countyID)
    }
  }

  years <- .capitalALL(years)
  categories <- .capitalALL(categories)

  url <- constructUseURL(
    years = years,
    stateCd = stateCd,
    countyCd = countyID,
    categories = categories
  )
  returned_data <- importRDB1(
    obs_url = url,
    convertType = convertType
  )

  # for total country data arriving in named rows
  if (transform) {
    cmmnt <- comment(returned_data)
    returned_data <- t(returned_data)
    colnames(returned_data) <- returned_data[1, ]
    returned_data <- as.data.frame(returned_data[-1, ], stringsAsFactors = FALSE)
    returned_data <- cbind(
      Year = as.integer(substr(rownames(returned_data), 2, 5)),
      returned_data
    )
    rownames(returned_data) <- NULL
    comment(returned_data) <- cmmnt
    if (!all(is.null(stateCd)) && all(nchar(stateCd) != 0)) {
      warning("transform = TRUE is only intended for national data")
    }
  }
  return(returned_data)
}

.capitalALL <- function(input) {
  if (any(grepl("(?i)all", input))) {
    input <- toupper(input)
  }
  return(input)
}
