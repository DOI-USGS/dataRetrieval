#' @name whatWQPsamples
#' @rdname wqpSpecials
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the
#' function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @export
#' @examples
#' \donttest{
#'
#' site1 <- whatWQPsamples(siteid = "USGS-01594440")
#'
#' type <- "Stream"
#'
#' sites <- whatWQPsamples(countycode = "US:55:025", siteType = type)
#'
#' lakeSites_samples <- whatWQPsamples(siteType = "Lake, Reservoir, Impoundment",
#'                                     countycode = "US:55:025")
#' }
whatWQPsamples <- function(..., 
                           convertType = TRUE) {
  values <- readWQPdots(...)

  values <- values$values

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }

  values <- sapply(values, function(x) utils::URLencode(x, reserved = TRUE))

  baseURL <- drURL("Activity", arg.list = values)

  baseURL <- appendDrURL(baseURL, mimeType = "tsv")

  withCallingHandlers(
    {
      retval <- importWQP(baseURL,
        convertType = convertType
      )
    },
    warning = function(w) {
      if (any(grepl("Number of rows returned not matched in header", w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- baseURL

  return(retval)
}

#' @name whatWQPmetrics
#' @rdname wqpSpecials
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE},
#' the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @export
#' @examples
#' \donttest{
#'
#' type <- "Stream"
#'
#' sites <- whatWQPmetrics(countycode = "US:55:025", siteType = type)
#' lakeSites_metrics <- whatWQPmetrics(siteType = "Lake, Reservoir, Impoundment",
#'                                     countycode = "US:55:025")
#' }
whatWQPmetrics <- function(..., 
                           convertType = TRUE) {
  values <- readWQPdots(...)

  values <- values$values

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }

  values <- sapply(values, function(x) utils::URLencode(x, reserved = TRUE))

  baseURL <- drURL("ActivityMetric", arg.list = values)

  baseURL <- appendDrURL(baseURL, mimeType = "tsv")

  withCallingHandlers(
    {
      retval <- importWQP(baseURL,
        convertType = convertType
      )
    },
    warning = function(w) {
      if (any(grepl("Number of rows returned not matched in header", w))) {
        invokeRestart("muffleWarning")
      }
    }
  )

  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- baseURL

  return(retval)
}


#' Data Available from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets
#' the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on
#' \url{https://www.waterqualitydata.us/webservices_documentation}.
#' The information returned from whatWQPdata describes the
#' available data at the WQP sites, and some metadata on the sites themselves.
#' For example, a row is returned for each individual site that fulfills this 
#' query. In that we can learn how many sampling activities and results 
#' are available for the query. It does not break those results down by any finer 
#' grain. For example, if you ask for "Nutrients" (characteristicGroup), you will
#' not learn what specific nutrients are available at that site. For that 
#' kind of data discovery see \code{readWQPsummary}.
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for
#' a complete list of options. A list of arguments can also be supplied.
#' One way to figure out how to construct a WQP query is to go to the "Advanced" 
#' form in the Water Quality Portal:
#' \url{https://www.waterqualitydata.us/#mimeType=csv&providers=NWIS&providers=STORET}
#' Use the form to discover what parameters are available. Once the query is 
#' set in the form, scroll down to the "Query URL". You will see the parameters
#' after "https://www.waterqualitydata.us/#". For example, if you chose "Nutrient"
#' in the Characteristic Group dropdown, you will see characteristicType=Nutrient
#' in the Query URL. The corresponding argument for dataRetrieval is
#' characteristicType = "Nutrient". dataRetrieval users do not need to include
#' mimeType, and providers is optional (these arguments are picked automatically).
#' @param saveFile path to save the incoming geojson output.
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @keywords data import WQP web service
#' @return A data frame based on the Water Quality Portal results.
#'
#' @export
#' @seealso whatWQPsites readWQPsummary readWQPdata
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' site1 <- whatWQPdata(siteid = "USGS-01594440")
#'
#' type <- "Stream"
#' sites <- whatWQPdata(countycode = "US:55:025", siteType = type)
#'
#' lakeSites <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment",
#'                          countycode = "US:55:025")
#' lakeSites_chars <- whatWQPdata(
#'   siteType = "Lake, Reservoir, Impoundment",
#'   countycode = "US:55:025", convertType = FALSE)
#' }
whatWQPdata <- function(..., saveFile = tempfile(),
                        convertType = TRUE) {
  values <- readWQPdots(...)

  wqp_message()
  
  values <- values$values

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }

  values <- sapply(values, function(x) utils::URLencode(x, reserved = TRUE))

  baseURL <- drURL("Station", arg.list = values)

  baseURL <- appendDrURL(baseURL, mimeType = "geojson")
  
  doc <- getWebServiceData(baseURL, httr::write_disk(saveFile))
  
  if (is.null(doc)) {
    return(invisible(NULL))
  }
  
  headerInfo <- attr(doc, "headerInfo")

  if (headerInfo$`total-site-count` == 0) {
    y <- data.frame(
      total_type = character(),
      lat = numeric(),
      lon = numeric(),
      ProviderName = character(),
      OrganizationIdentifier = character(),
      OrganizationFormalName = character(),
      MonitoringLocationIdentifier = character(),
      MonitoringLocationName = character(),
      MonitoringLocationTypeName = character(),
      ResolvedMonitoringLocationTypeName = character(),
      HUCEightDigitCode = character(),
      siteUrl = character(),
      activityCount = numeric(),
      resultCount = numeric(),
      StateName = character(),
      CountyName = character(),
      stringsAsFactors = FALSE
    )
    if (!convertType) {
      y <- data.frame(lapply(y, as.character), stringsAsFactors = FALSE)
    }
  } else {

    retval <- as.data.frame(jsonlite::fromJSON(saveFile), stringsAsFactors = FALSE)
    df_cols <- as.integer(which(sapply(retval, class) == "data.frame"))
    y <- retval[, -df_cols]

    for (i in df_cols) {
      y <- cbind(y, retval[[i]])
    }

    if (convertType) {
      y[, grep("Count$", names(y))] <- sapply(y[, grep("Count$", names(y))], as.numeric)
    }

    names(y)[names(y) == "type"] <- paste("type",
      letters[seq_along(names(y)[names(y) == "type"])],
      sep = "_"
    )

    if (all(c("type_a", "type_b", "features.type") %in% names(y))) {
      y$total_type <- paste(y$type_a, y$features.type, y$type_b)
      y$type_a <- NULL
      if (all(y$type_b == "Point")) {
        y$lon <- sapply(y$coordinates, function(x) x[[1]][1])
        y$lat <- sapply(y$coordinates, function(x) x[[2]][1])
      }
      y$type_b <- NULL
      y$coordinates <- NULL
      y$features.type <- NULL
      y <- y[, c("total_type", "lat", "lon", names(y)[!(names(y) %in% c("total_type", "lat", "lon"))])]
    }
  }

  if (!convertType) {
    y <- data.frame(lapply(y, as.character), stringsAsFactors = FALSE)
  }

  attr(y, "queryTime") <- Sys.time()
  attr(y, "url") <- baseURL
  attr(y, "file") <- saveFile
  return(y)
}
