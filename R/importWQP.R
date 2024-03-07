

#' Basic Water Quality Portal Data parser
#'
#' Imports data from the Water Quality Portal based on a specified url.
#'
#' @param obs_url character URL to Water Quality Portal#' @keywords data import USGS web service
#' @param zip logical to request data via downloading zip file. Default set to TRUE.
#' @param tz character to set timezone attribute of datetime. Default is UTC
#' (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values include "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua",
#' "America/Phoenix", and "America/Metlakatla"
#' @param csv logical. Is the data coming back with a csv or tsv format. Default is \code{FALSE}.
#' Currently, the summary service does not support tsv, for other services tsv is the safer choice.
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @param checkHeader logical, defaults to \code{TRUE}. If \code{TRUE}, the code
#' will check that the curl header response for number of rows matches the actual
#' number of rows. While \code{TRUE} is much more robust, \code{FALSE} is much faster,
#' since the WQP database does not need to do as much pre-calculations to add to the 
#' header.
#' @return retval dataframe raw data returned from the Water Quality Portal. Additionally,
#' a POSIXct dateTime column is supplied for
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
#' rawSampleURL_NoZip_char <- importWQP(rawSampleURL_NoZip, zip = FALSE, convertType = FALSE)
#'
#' rawSample2 <- importWQP(rawSampleURL_NoZip, zip = FALSE)
#'
#' STORETex <- constructWQPURL("WIDNR_WQX-10032762", "Specific conductance", "", "")
#'
#' STORETdata <- importWQP(STORETex)
#'
#' STORETdata_char <- importWQP(STORETex, convertType = FALSE)
#' }
#'
importWQP <- function(obs_url, zip = TRUE, tz = "UTC",
                      csv = FALSE, 
                      convertType = TRUE,
                      checkHeader = TRUE) {
  if (tz != "") {
    tz <- match.arg(tz, OlsonNames())
  } else {
    tz <- "UTC"
  }

  if (!file.exists(obs_url)) {
    if(!checkHeader){
      obs_url <- paste0(obs_url, "&counts=no")
    }
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
    
    if(checkHeader){
      headerInfo[grep("-count", names(headerInfo))] <- as.numeric(headerInfo[grep("-count", names(headerInfo))])
  
      totalPossible <- sum(unlist(headerInfo[grep("-count", names(headerInfo))]), na.rm = TRUE)

    } else {
      totalPossible <- Inf
    }
  } else {
    if (zip) {
      doc <- utils::unzip(obs_url)
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
  
  if (checkHeader & !file.exists(obs_url)) {
    actualNumReturned <- nrow(retval)
    
    if (!(actualNumReturned %in% unlist(headerInfo[grep("-count", names(headerInfo))]))) {
      warning("Number of rows returned not matched in header")
    }
  }
  
  if(convertType){
    retval <- parse_WQP(retval, tz)
  } 
  
  names(retval)[grep("/", names(retval))] <- gsub("/", ".", names(retval)[grep("/", names(retval))])
  
  return(retval)
  
}


parse_WQP <- function(retval, tz){

  valueCols <- names(retval)[grep("MeasureValue", names(retval))]
  countCols <- names(retval)[grep("Count", names(retval))]
  yearCols <- names(retval)[grep("Year", names(retval))]
  
  numberColumns <- unique(c(valueCols, countCols, yearCols))
  numberColumns <- numberColumns[!grepl("Code", numberColumns)]
  
  for (numberCol in numberColumns) {
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
  
  offsetLibrary$offset[is.na(offsetLibrary$code)] <- NA
  
  if (length(grep("ActivityStartTime", names(retval))) > 0) {
    
    # Time zones to characters:
    if (length(grep("TimeZoneCode", names(retval))) > 0 &&
        any(lapply(retval[, grep("TimeZoneCode", names(retval))], class) == "logical")) {
      tzCols <- grep("TimeZoneCode", names(retval))
      retval[, tzCols] <- sapply(retval[, tzCols], as.character)
    }
    
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
      # if we're going to sort, here's where we'd do it:
      retval <- retval[order(retval$ActivityStartDateTime),]
    }
    
    if (all(c("ActivityEndDate", "ActivityEndTime/Time") %in% names(retval))) {
      retval$ActivityEndDateTime <- paste(retval$ActivityEndDate, retval$`ActivityEndTime/Time`)
      retval$ActivityEndDateTime <- lubridate::fast_strptime(
        retval$ActivityEndDateTime,
        "%Y-%m-%d %H:%M:%S"
      ) + 60 * 60 * retval$timeZoneStart
      attr(retval$ActivityEndDateTime, "tzone") <- tz
    }
  }
  
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
    url <- paste0(url, "&sorted=", strsplit(split[[1]][2], "sorted=", fixed = TRUE)[[1]][2])
  }

  if (zip) {
    url <- paste0(url, "&zip=yes")
  }

  return(url)
}
