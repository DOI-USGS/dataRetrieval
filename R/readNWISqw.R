#' Raw Data Import for USGS NWIS QW Data
#'
#' Deprecated function. USGS NWIS QW services will be discontinued, when that happens
#' this function will be removed from dataRetrieval. Please use\code{\link{readWQPqw}} or 
#' \code{\link{readWQPdata}} instead.
#' 
#' As of March 11, 2024, NWIS discrete water quality services are "frozen": 
#' any public data retrieval will not include any new data. 
#' 
#' Contact CompTools@usgs.gov with additional questions!
#'
#' @param siteNumbers character of USGS site numbers.  This is usually an 8 digit number
#' @param parameterCd character that contains the code for a parameter
#' group, or a character vector of 5-digit parameter codes. See \bold{Details}.
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' Default is "" which indicates
#' retrieval for the earliest possible record. Date arguments are always specified in local time.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD.
#' Default is "" which indicates
#' retrieval for the latest possible record. Date arguments are always specified in local time.
#' @param expanded logical defaults to \code{TRUE}. If \code{TRUE}, retrieves
#' additional information. Expanded data includes
#' remark_cd (remark code), result_va (result value), val_qual_tx
#' (result value qualifier code), meth_cd (method code),
#' dqi_cd (data-quality indicator code), rpt_lev_va (reporting level), and
#' rpt_lev_cd (reporting level type). If \code{FALSE},
#' only returns remark_cd (remark code) and result_va (result value).
#' Expanded = \code{FALSE} will not give
#' sufficient information for unbiased statistical analysis.
#' @param reshape logical, reshape the expanded data. If \code{TRUE}, then return
#' a wide data frame with all water-quality in a single row for each sample.
#' If \code{FALSE} (default), then return a long data frame with each water-quality result in a single row. This
#' argument is only applicable to expanded data. Data requested using \code{expanded=FALSE}
#' is always returned in the wide format.
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the
#' date times to UTC, properly accounting for daylight savings times based on the
#' data's provided tz_cd column.
#' Possible values to provide are "America/New_York", "America/Chicago",
#' "America/Denver", "America/Los_Angeles", "America/Anchorage", as well as the
#' following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica", "America/Managua", "America/Phoenix", and "America/Metlakatla".
#' See also  \code{OlsonNames()}
#' for more information on time zones.
#' @keywords data import USGS web service
#' @return A data frame not including USGS data newer than March 11, 2024.
#'
#' @export
#' @seealso \code{\link{readWQPdata}}, \code{\link{whatWQPsites}},
#' \code{\link{readWQPqw}}, \code{\link{constructNWISURL}}
readNWISqw <- function(siteNumbers,
                       parameterCd,
                       startDate = "",
                       endDate = "",
                       expanded = TRUE,
                       reshape = FALSE,
                       tz = "UTC") {
  .Deprecated(
    new = "readWQPqw", package = "dataRetrieval",
    msg = nwis_message()
  )

  pgrp <- c(
    "INF", "PHY", "INM", "INN", "NUT", "MBI", "BIO", "IMM", "IMN", "TOX",
    "OPE", "OPC", "OOT", "RAD", "XXX", "SED", "POP",
    "ISO", "OTH", "HAB"
  )

  if (any(parameterCd == "all") || any(parameterCd == "All")) {
    siteNumbers <- paste(siteNumbers, collapse = ",")
    url <- paste0(
      "https://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no= ", siteNumbers,
      "&sort_key=site_no&group_key=NONE&inventory_output=0",
      "&begin_date=", startDate, "&end_date=", endDate,
      "&TZoutput=0",
      "&radio_parm_cds=all_parm_cds&qw_attributes=0&format=rdb",
      "&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD",
      "&rdb_compression=value&list_of_search_criteria=multiple_site_no"
    )
  } else if (all(parameterCd %in% pgrp)) {
    siteNumbers <- paste(siteNumbers, collapse = ",")
    groups <- paste(parameterCd, collapse = ",")
    url <- paste0(
      "https://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=", siteNumbers,
      "&sort_key=site_no&group_key=NONE&inventory_output=0",
      "&begin_date=", startDate, "&end_date=", endDate,
      "&TZoutput=0&param_group=", groups,
      "&qw_attributes=0&format=rdb",
      "&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD",
      "&rdb_compression=value&list_of_search_criteria=multiple_site_no"
    )
  } else {
    url <- constructNWISURL(siteNumbers,
      parameterCd,
      startDate,
      endDate, "qw",
      expanded = expanded
    )
  }

  data <- importRDB1(url, asDateTime = TRUE, tz = tz)

  url <- attr(data, "url")
  comment <- attr(data, "comment")
  queryTime <- attr(data, "queryTime")
  header <- attr(data, "headerInfo")

  if (reshape) {
    if (expanded) {
      columnsToMelt <- c(
        "agency_cd", "site_no", "sample_dt", "sample_tm",
        "sample_end_dt", "sample_end_tm", "sample_start_time_datum_cd", "tm_datum_rlbty_cd",
        "parm_cd", "startDateTime", "endDateTime", "coll_ent_cd", "medium_cd", "project_cd",
        "aqfr_cd", "tu_id", "body_part_id", "hyd_cond_cd", "samp_type_cd",
        "hyd_event_cd", "sample_lab_cm_tx", "tz_cd", "startDateTime", "endDateTime",
        "sample_start_time_datum_cd_reported", "sample_end_time_datum_cd_reported"
      )
      measureCols <- names(data)[!(names(data) %in% columnsToMelt)]
      columnsToMelt <- names(data)[(names(data) %in% columnsToMelt)]
      dataWithPcodes <- data[data$parm_cd != "", ]
      if (sum(data$parm_cd == "", na.rm = TRUE)> 0) {
        warning("Some or all data returned without pCodes, those data will not be included in reshape")
      }
      original_start_dates <- dataWithPcodes$startDateTime
      original_end_dates <- dataWithPcodes$endDateTime

      parameterCd <- unique(dataWithPcodes$parm_cd)
      idcols <- names(dataWithPcodes)[!(names(dataWithPcodes) %in% c(measureCols, "parm_cd"))]

      dataWithPcodes[, idcols] <- lapply(dataWithPcodes[, idcols], function(x) as.factor(x))
      dataWithPcodes[, idcols] <- lapply(dataWithPcodes[, idcols], function(x) addNA(x))

      wide2 <- stats::reshape(dataWithPcodes,
        idvar = idcols,
        timevar = "parm_cd",
        v.names = measureCols,
        direction = "wide"
      )
      names(wide2) <- gsub("\\.", "_", names(wide2))
      idcols_factors <- idcols[unname(sapply(wide2[idcols], class) == "factor")]
      idcols_dates <- idcols[grep("_dt", idcols)]

      wide2[, idcols_factors] <- sapply(wide2[, idcols_factors], as.character)
      wide2[, idcols_dates] <- lapply(wide2[, idcols_dates], as.Date)
      other_dates <- names(wide2)[grep("_dt", names(wide2))]
      other_dates <- other_dates[!(other_dates %in% idcols_dates)]
      wide2[, other_dates] <- lapply(wide2[, other_dates], function(x) as.Date(strptime(x, format = "%Y%m%d")))
      if ("startDateTime" %in% names(wide2)) {
        wide2[, "startDateTime"] <- as.POSIXct(wide2[, "startDateTime"], tz = attr(original_start_dates, "tzone"))
      }
      if ("endDateTime" %in% names(wide2)) {
        wide2[, "endDateTime"] <- as.POSIXct(wide2[, "endDateTime"], tz = attr(original_end_dates, "tzone"))
      }

      data <- wide2
    } else {
      warning("Reshape can only be used with expanded data. Reshape request will be ignored.")
    }
  } else {
    parameterCd <- unique(data$parm_cd)
  }

  if (exists("siteNumbers") && all(!(is.na(siteNumbers))) && length(siteNumbers) > 0) {
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
  }

  if (exists("parameterCd") && any(!is.na(parameterCd)) &&
    length(parameterCd) > 0) {
    parameterCd <- parameterCd[!is.na(parameterCd)]
    parameterCd <- parameterCd[parameterCd != ""]
    varInfo <- readNWISpCode(parameterCd)
    attr(data, "variableInfo") <- varInfo
  }

  attr(data, "statisticInfo") <- NULL

  attr(data, "url") <- url
  attr(data, "comment") <- comment
  attr(data, "queryTime") <- queryTime
  attr(data, "header") <- header

  return(data)
}
