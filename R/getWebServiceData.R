#' Function to return data from web services
#'
#' This function accepts a url parameter, and returns the raw data. The function enhances
#' \code{\link[httr]{GET}} with more informative error messages.
#'
#' @param obs_url character containing the url for the retrieval
#' @param \dots information to pass to header request
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_child
#' @importFrom xml2 read_xml
#' @export
#' @return raw data from web services
#' @examplesIf is_dataRetrieval_user()
#' siteNumber <- "02177000"
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' offering <- "00003"
#' property <- "00060"
#' obs_url <- constructNWISURL(siteNumber, property, startDate, endDate, "dv")
#' \donttest{
#' rawData <- getWebServiceData(obs_url)
#' }
getWebServiceData <- function(obs_url, ...) {
  if (!has_internet_2(obs_url)) {
    message("No internet connection.")
    return(invisible(NULL))
  }

  returnedList <- retryGetOrPost(obs_url, ...)

  if (httr::status_code(returnedList) == 400) {
    if (httr::has_content(returnedList)) {
      response400 <- httr::content(returnedList, type = "text", encoding = "UTF-8")
      statusReport <- xml_text(xml_child(read_xml(response400), 2)) # making assumption that - body is second node
      statusMsg <- gsub(pattern = ", server=.*", replacement = "", x = statusReport)
      message(statusMsg)
    } else {
      httr::message_for_status(returnedList)
      warning_message <- httr::headers(returnedList)
      if ("warning" %in% names(warning_message)) {
        warning_message <- warning_message$warning
        message(warning_message)
      }
    }
    return(invisible(NULL))
  } else if (httr::status_code(returnedList) != 200) {
    message("For: ", obs_url, "\n")
    httr::message_for_status(returnedList)
    return(invisible(NULL))
  } else {
    headerInfo <- httr::headers(returnedList)

    if (!"content-type" %in% names(headerInfo)) {
      message("Unknown content, returning NULL")
      return(invisible(NULL))
    }

    if (headerInfo$`content-type` %in% c(
      "text/tab-separated-values;charset=UTF-8",
      "text/csv;charset=UTF-8"
    )) {
      returnedDoc <- httr::content(returnedList, type = "text", encoding = "UTF-8")
    } else if (headerInfo$`content-type` %in%
      c(
        "application/zip",
        "application/zip;charset=UTF-8",
        "application/vnd.geo+json;charset=UTF-8"
      )) {
      returnedDoc <- returnedList
    } else if (headerInfo$`content-type` %in% c(
      "text/html",
      "text/html; charset=UTF-8"
    )) {
      txt <- readLines(returnedList$content)
      message(txt)
      return(txt)
    } else {
      returnedDoc <- httr::content(returnedList, encoding = "UTF-8")
      if (all(grepl("No sites/data found using the selection criteria specified", returnedDoc))) {
        message(returnedDoc)
      }
      if (headerInfo$`content-type` == "text/xml") {
        if (xml_name(read_xml(returnedList)) == "ExceptionReport") {
          statusReport <- tryCatch({
            xml_text(xml_child(read_xml(returnedList)))
          })
          if (grepl("No feature found", statusReport)) {
            message(statusReport)
          }
        }
      }
    }

    attr(returnedDoc, "headerInfo") <- headerInfo

    return(returnedDoc)
  }
}

#' Create user agent
#'
#' @keywords internal
default_ua <- function() {
  versions <- c(
    libcurl = curl::curl_version()$version,
    httr = as.character(packageVersion("httr")),
    dataRetrieval = as.character(packageVersion("dataRetrieval"))
  )

  ua <- paste0(names(versions), "/", versions, collapse = " ")

  if ("UA.dataRetrieval" %in% names(options)) {
    ua <- paste0(ua, "/", options()[["UA.dataRetrieval"]])
  }

  return(ua)
}

#' has_internet2
#'
#' Function to check for internet even if the user
#' is behind a proxy
#'
#' @keywords internal
#' @param obs_url character obs_url to check
has_internet_2 <- function(obs_url) {
  host <- gsub("^https://(?:www[.])?([^/]*).*$", "\\1", obs_url)

  !is.null(curl::nslookup(host, error = FALSE))
}

#' getting header information from a WQP query
#'
#' @param url the query url
getQuerySummary <- function(url) {
  queryHEAD <- httr::HEAD(url)
  retquery <- httr::headers(queryHEAD)

  retquery[grep("-count", names(retquery))] <- as.numeric(retquery[grep("-count", names(retquery))])

  if ("date" %in% names(retquery)) {
    retquery$date <- as.Date(retquery$date, format = "%a, %d %b %Y %H:%M:%S")
  }

  return(retquery)
}

retryGetOrPost <- function(obs_url, ...) {
  resp <- NULL
  if (nchar(obs_url) < 2048 || grepl(pattern = "ngwmn", x = obs_url)) {
    resp <- httr::RETRY("GET", obs_url, ..., httr::user_agent(default_ua()))
  } else {
    split <- strsplit(obs_url, "?", fixed = TRUE)
    obs_url <- split[[1]][1]
    query <- split[[1]][2]

    resp <- httr::RETRY("POST", obs_url, ...,
      body = query,
      httr::content_type("application/x-www-form-urlencoded"),
      httr::user_agent(default_ua())
    )
  }
  return(resp)
}
