

#' Basic Water Quality Portal Data parser
#'
#' Imports data from the Water Quality Portal based on a specified url.
#'
#' @param obs_url character URL to Water Quality Portal#' @keywords data import USGS web service
#' @param zip logical to request data via downloading zip file. Default set to TRUE.
#' @param tz character to set timezone attribute of datetime. Default is UTC
#' (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values include "America/New_York", "America/Chicago", "America/Denver",
#' "America/Los_Angeles", "America/Anchorage", "America/Honolulu", "America/Jamaica", "America/Managua",
#' "America/Phoenix", and "America/Metlakatla"
#' @param csv logical. Is the data coming back with a csv or tsv format. Default is
#' \code{FALSE}. Currently, the
#' summary service does not support tsv, for other services tsv is the safer choice.
#' @return retval dataframe raw data returned from the Water Quality Portal.
#' Additionally, a POSIXct dateTime column is supplied for
#' start and end times, and converted to UTC. See
#' \url{https://www.waterqualitydata.us/portal_userguide/} for more information.
#' @export
#' @seealso \code{\link{readWQPdata}}, \code{\link{readWQPqw}}, \code{\link{whatWQPsites}}
#' @examplesIf is_dataRetrieval_user()
#' # These examples require an internet connection to run
#'
#' ## Examples take longer than 5 seconds:
#' \donttest{
#' rawSampleURL <- constructWQPURL("USGS-01594440", "01075", "", "")
#'
#' rawSample <- importWQP(rawSampleURL)
#'
#' rawSampleURL_NoZip <- constructWQPURL("USGS-01594440", "01075", "", "", zip = FALSE)
#'
#' rawSample2 <- importWQP(rawSampleURL_NoZip, zip = FALSE)
#'
#' STORETex <- constructWQPURL("WIDNR_WQX-10032762", "Specific conductance", "", "")
#'
#' STORETdata <- importWQP(STORETex)
#' }
#'
importWQP <- function(obs_url, zip = TRUE, tz = "UTC",
                      csv = FALSE) {
  if (tz != "") {
    tz <- match.arg(tz, OlsonNames())
  } else {
    tz <- "UTC"
  }

  if (!file.exists(obs_url)) {
    if (zip) {
      temp <- tempfile()
      temp <- paste0(temp, ".zip")

      doc <- getWebServiceData(
        obs_url,
        httr::write_disk(temp),
        httr::accept("application/zip")
      )
      if (is.null(doc)) {
        return(invisible(NULL))
      }
      headerInfo <- httr::headers(doc)
      doc <- utils::unzip(temp, exdir = tempdir())
      unlink(temp)
      on.exit(unlink(doc))
    } else {
      doc <- getWebServiceData(
        obs_url,
        httr::accept("text/tsv")
      )
      if (is.null(doc)) {
        return(invisible(NULL))
      }
      headerInfo <- attr(doc, "headerInfo")
    }

    headerInfo[grep("-count", names(headerInfo))] <- as.numeric(headerInfo[grep("-count", names(headerInfo))])

    totalPossible <- sum(unlist(headerInfo[grep("-count", names(headerInfo))]), na.rm = TRUE)

    if (is.na(totalPossible) || totalPossible == 0) {
      for (i in grep("Warning", names(headerInfo))) {
        warning(headerInfo[i])
      }
      emptyReturn <- data.frame(
        OrganizationIdentifier = character(),
        OrganizationFormalName = character(),
        ActivityIdentifier = character(),
        ActivityTypeCode = character(),
        ActivityMediaName = character(),
        ActivityMediaSubdivisionName = character(),
        ActivityStartDate = as.Date(x = integer(), origin = "1970-01-01"),
        ActivityStartTime.Time = character(),
        ActivityStartTime.TimeZoneCode = character(),
        ActivityEndDate = as.Date(x = integer(), origin = "1970-01-01"),
        ActivityEndTime.Time = character(),
        ActivityEndTime.TimeZoneCode = character(),
        ActivityDepthHeightMeasure.MeasureValue = numeric(),
        ActivityDepthHeightMeasure.MeasureUnitCode = character(),
        ActivityDepthAltitudeReferencePointText = character(),
        ActivityTopDepthHeightMeasure.MeasureValue = numeric(),
        ActivityTopDepthHeightMeasure.MeasureUnitCode = character(),
        ActivityBottomDepthHeightMeasure.MeasureValue = numeric(),
        ActivityBottomDepthHeightMeasure.MeasureUnitCode = character(),
        ProjectIdentifier = character(),
        ActivityConductingOrganizationText = character(),
        MonitoringLocationIdentifier = character(),
        ActivityCommentText = character(),
        SampleAquifer = character(),
        HydrologicCondition = character(),
        HydrologicEvent = character(),
        SampleCollectionMethod.MethodIdentifier = character(),
        SampleCollectionMethod.MethodIdentifierContext = character(),
        SampleCollectionMethod.MethodName = character(),
        SampleCollectionEquipmentName = character(),
        ResultDetectionConditionText = character(),
        CharacteristicName = character(),
        ResultSampleFractionText = character(),
        ResultMeasureValue = numeric(),
        ResultMeasure.MeasureUnitCode = character(),
        MeasureQualifierCode = character(),
        ResultStatusIdentifier = character(),
        StatisticalBaseCode = character(),
        ResultValueTypeName = character(),
        ResultWeightBasisText = character(),
        ResultTimeBasisText = character(),
        ResultTemperatureBasisText = character(),
        ResultParticleSizeBasisText = character(),
        PrecisionValue = numeric(),
        ResultCommentText = character(),
        USGSPCode = character(),
        ResultDepthHeightMeasure.MeasureValue = numeric(),
        ResultDepthHeightMeasure.MeasureUnitCode = character(),
        ResultDepthAltitudeReferencePointText = character(),
        SubjectTaxonomicName = character(),
        SampleTissueAnatomyName = character(),
        ResultAnalyticalMethod.MethodIdentifier = character(),
        ResultAnalyticalMethod.MethodIdentifierContext = character(),
        ResultAnalyticalMethod.MethodName = character(),
        MethodDescriptionText = character(),
        LaboratoryName = character(),
        AnalysisStartDate = as.Date(x = integer(), origin = "1970-01-01"),
        ResultLaboratoryCommentText = character(),
        DetectionQuantitationLimitTypeName = character(),
        DetectionQuantitationLimitMeasure.MeasureValue = numeric(),
        DetectionQuantitationLimitMeasure.MeasureUnitCode = character(),
        PreparationStartDate = as.Date(x = integer(), origin = "1970-01-01"),
        ProviderName = character(),
        ActivityStartDateTime = as.POSIXct(integer()),
        ActivityEndDateTime = as.POSIXct(integer())
      )

      attr(emptyReturn$ActivityStartDateTime, "tzone") <- tz
      attr(emptyReturn$ActivityEndDateTime, "tzone") <- tz

      attr(emptyReturn, "headerInfo") <- headerInfo
      return(emptyReturn)
    }
  } else {
    if (zip) {
      doc <- unzip(obs_url)
    } else {
      doc <- obs_url
    }
  }

  retval <- suppressWarnings(readr::read_delim(doc,
    col_types = readr::cols(.default = "c"),
    quote = ifelse(csv, '\"', ""),
    delim = ifelse(csv, ",", "\t"),
    guess_max = totalPossible
  ))

  if (!file.exists(obs_url)) {
    actualNumReturned <- nrow(retval)

    if (!(actualNumReturned %in% unlist(headerInfo[grep("-count", names(headerInfo))]))) {
      warning("Number of rows returned not matched in header")
    }
  }

  valueCols <- names(retval)[grep("MeasureValue", names(retval))]
  countCols <- names(retval)[grep("Count", names(retval))]
  yearCols <- names(retval)[grep("Year", names(retval))]

  for (numberCol in unique(c(valueCols, countCols, yearCols))) {
    suppressWarnings({
      val <- tryCatch(as.numeric(retval[[numberCol]]),
        warning = function(w) w
      )
      # we don't want to convert it to numeric if there are non-numeric chars
      # If we leave it to the user, it will probably break a lot of code
      if (!"warning" %in% class(val)) {
        retval[[numberCol]] <- val
      }
    })
  }



  if (length(grep("ActivityStartTime", names(retval))) > 0) {

    # Time zones to characters:
    if (length(grep("TimeZoneCode", names(retval))) > 0 &&
      any(lapply(retval[, grep("TimeZoneCode", names(retval))], class) == "logical")) {
      tzCols <- grep("TimeZoneCode", names(retval))
      retval[, tzCols] <- sapply(retval[, tzCols], as.character)
    }

    offsetLibrary <- data.frame(
      offset = c(5, 4, 6, 5, 7, 6, 8, 7, 9, 8, 10, 10, 0, NA, 0, 0),
      code = c(
        "EST", "EDT", "CST", "CDT", "MST",
        "MDT", "PST", "PDT", "AKST", "AKDT", "HAST", "HST",
        "", NA, "UTC", "GMT"
      ),
      stringsAsFactors = FALSE
    )
    original_order <- names(retval)
    if ("ActivityStartTime/TimeZoneCode" %in% names(retval)) {
      retval <- merge(
        x = retval,
        y = offsetLibrary,
        by.x = "ActivityStartTime/TimeZoneCode",
        by.y = "code",
        all.x = TRUE
      )
    }

    names(retval)[names(retval) == "offset"] <- "timeZoneStart"
    retval <- retval[, c(original_order, "timeZoneStart")]

    if ("ActivityEndTime/TimeZoneCode" %in% names(retval)) {
      retval <- merge(
        x = retval,
        y = offsetLibrary,
        by.x = "ActivityEndTime/TimeZoneCode",
        by.y = "code",
        all.x = TRUE
      )
      names(retval)[names(retval) == "offset"] <- "timeZoneEnd"
      retval <- retval[, c(original_order, "timeZoneStart", "timeZoneEnd")]
    }


    dateCols <- c("ActivityStartDate", "ActivityEndDate", "AnalysisStartDate", "PreparationStartDate")

    for (i in dateCols) {
      if (i %in% names(retval)) {
        retval[, i] <- suppressWarnings(as.Date(lubridate::parse_date_time(retval[[i]], c("Ymd", "mdY"))))
      }
    }

    if (all(c("ActivityStartDate", "ActivityStartTime/Time") %in% names(retval))) {
      retval$ActivityStartDateTime <- paste(retval$ActivityStartDate, retval$`ActivityStartTime/Time`)
      retval$ActivityStartDateTime <- lubridate::fast_strptime(retval$ActivityStartDateTime, "%Y-%m-%d %H:%M:%S") +
        60 * 60 * retval$timeZoneStart
      attr(retval$ActivityStartDateTime, "tzone") <- tz
    }

    if (all(c("ActivityEndDate", "ActivityEndTime/Time") %in% names(retval))) {
      retval$ActivityEndDateTime <- paste(retval$ActivityEndDate, retval$`ActivityEndTime/Time`)
      retval$ActivityEndDateTime <- lubridate::fast_strptime(retval$ActivityEndDateTime, "%Y-%m-%d %H:%M:%S") +
        60 * 60 * retval$timeZoneStart
      attr(retval$ActivityEndDateTime, "tzone") <- tz
    }

    retval <- retval[, names(retval)[!(names(retval) %in% c("timeZoneEnd", "timeZoneStart"))]]
  }
  names(retval)[grep("/", names(retval))] <- gsub("/", ".", names(retval)[grep("/", names(retval))])

  return(retval)
}


post_url <- function(obs_url, zip, csv = FALSE) {
  split <- strsplit(obs_url, "?", fixed = TRUE)

  url <- split[[1]][1]
  if (csv) {
    url <- paste0(url, "?mimeType=csv")
  } else {
    url <- paste0(url, "?mimeType=tsv")
  }

  if (grepl("sorted", split[[1]][2])) {
    url <- paste0(url, "&sorted=", strsplit(split[[1]][2],
      "sorted=",
      fixed = TRUE
    )[[1]][2])
  }

  if (zip) {
    url <- paste0(url, "&zip=yes")
  }

  return(url)
}
