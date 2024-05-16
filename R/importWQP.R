

#' Basic Water Quality Portal Data parser
#'
#' Imports data from the Water Quality Portal based on a specified url.
#'
#' @param obs_url character URL to Water Quality Portal#' @keywords data import USGS web service
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
#'
#' STORETex <- constructWQPURL("WIDNR_WQX-10032762", "Specific conductance", "", "")
#'
#' STORETdata <- importWQP(STORETex)
#'
#' STORETdata_char <- importWQP(STORETex, convertType = FALSE)
#' }
#'
importWQP <- function(obs_url, tz = "UTC",
                      csv = TRUE, 
                      convertType = TRUE) {
  if (tz != "") {
    tz <- match.arg(tz, OlsonNames())
  } else {
    tz <- "UTC"
  }

  if (!file.exists(obs_url)) {
    
    doc <- getWebServiceData(
      obs_url,
      httr::accept("text/tsv")
    )
    if (is.null(doc)) {
      return(invisible(NULL))
    }
    headerInfo <- attr(doc, "headerInfo")
    
    totalPossible <- Inf

  } else {
    doc <- obs_url
  }
  
  retval <- suppressWarnings(readr::read_delim(doc,
                                               col_types = readr::cols(.default = "c"),
                                               quote = ifelse(csv, '\"', ""),
                                               delim = ifelse(csv, ",", "\t"),
                                               guess_max = totalPossible
  ))
  
  names(retval)[grep("/", names(retval))] <- gsub("/", ".", names(retval)[grep("/", names(retval))])
  
  if(convertType){
    retval <- parse_WQP(retval, tz)
  } 
  
  return(retval)
  
}

#' Convert WQP columns to correct types
#' 
#' Takes the character results and converts to numeric and dates.
#' 
#' @param retval Data frame from WQP
#' @param tz character to set timezone attribute of datetime. Default is UTC
#' (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values include "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua",
#' "America/Phoenix", and "America/Metlakatla"
#' 
#' @export
#' @return data frame retval with converted columns
#' 
#' @examplesIf is_dataRetrieval_user()
#' # These examples require an internet connection to run
#' rawSampleURL <- constructWQPURL("USGS-01594440", "01075", "", "")
#' 
#' ## Examples take longer than 5 seconds:
#' 
#' \donttest{
#'
#' rawSample <- importWQP(rawSampleURL, convertType = FALSE)
#' convertedSample <- parse_WQP(rawSample)
#' 
#' } 
#' 
parse_WQP <- function(retval, tz = "UTC"){

  valueCols <- names(retval)[grep("Value", names(retval))]
  countCols <- names(retval)[grep("Count", names(retval))]
  yearCols <- names(retval)[grep("Year", names(retval))]
  dateCols <- names(retval)[grep("Date", names(retval))]
  
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
  
  dateCols_to_convert <- NA
  for(date_col in dateCols){
    
    time_col <- gsub("Date", "Time", date_col)
    tz_col <- gsub("Date", "TimeZone", date_col)
    if(all(c(date_col, time_col, tz_col) %in% names(retval))){
      if(!all(is.na(retval[[date_col]]))){
        retval <- create_dateTime(retval, 
                                 date_col = date_col,
                                 time_col = time_col,
                                 tz_col = tz_col,
                                 tz = tz)
      }
    } else {
      dateCols_to_convert <- c(dateCols_to_convert, date_col)
    }
  }
  
  dateCols_to_convert <- dateCols_to_convert[!is.na(dateCols_to_convert)]
  if(length(dateCols_to_convert) > 0){
    for (i in dateCols_to_convert) {
      if (i %in% names(retval)) {
        retval[, i] <- suppressWarnings(as.Date(lubridate::parse_date_time(retval[[i]], c("Ymd", "mdY"))))
      }
    }
  }
  
  return(retval)
}

create_dateTime <- function(df, date_col, time_col, tz_col, tz){
  # Difference in behavior between NWIS and WQP
  offsetLibrary$offset[is.na(offsetLibrary$code)] <- NA
  original_order <- names(df)
  
  df <- merge(
    x = df,
    y = offsetLibrary,
    by.x = tz_col,
    by.y = "code",
    all.x = TRUE
  )
  
  df$dateTime <- paste(df[[date_col]], df[[time_col]])
  df$dateTime <- lubridate::fast_strptime(
    df$dateTime,
    "%Y-%m-%d %H:%M:%S"
  ) + 60 * 60 * df$offset
  
  attr(df$dateTime, "tzone") <- tz
  
  df[[date_col]] <- suppressWarnings(as.Date(lubridate::parse_date_time(df[[date_col]], c("Ymd", "mdY"))))
  
  df <- df[, c(original_order, "offset", "dateTime")]
  
  names(df)[names(df) == "offset"] <- paste0(tz_col, "_offset")
  names(df)[names(df) == "dateTime"] <- paste0(date_col, "Time")
  
  return(df)
}

post_url <- function(obs_url, csv = FALSE) {
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

  return(url)
}
