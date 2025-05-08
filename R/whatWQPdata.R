#' @name whatWQPsamples
#' @rdname wqpSpecials
#' @export
#' @return A data frame with information on the sampling activity
#' available from the Water Quality Portal for the query parameters.
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
                           convertType = TRUE,
                           legacy = TRUE) {
  values <- readWQPdots(..., legacy = legacy)

  values <- values[["values"]]

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }

  if(legacy){
    baseURL <- httr2::request(pkg.env[["Activity"]])
  } else {
    baseURL <- httr2::request(pkg.env[["ActivityWQX3"]])
  }
  POST = FALSE
  if(!legacy){
    baseURL <- httr2::req_url_query(baseURL, !!!values, 
                                    .multi = "explode")
  } else {
    if("siteid" %in% names(values)){
      if(length(values[["siteid"]]) > 1){
        sites <- values[["siteid"]]
        POST = nchar(paste0(sites, collapse = "")) > 2048
        
        baseURL <- get_or_post(baseURL, POST = POST,
                               siteid = sites,
                               .multi = function(x) paste0(x, collapse = ";"))
        
        values <- values[names(values) != "siteid"]
      }
    }
    baseURL <- get_or_post(baseURL, 
                           POST = POST,
                           !!!values, 
                           .multi = "explode")
  }
  
  retval <- importWQP(baseURL,
                      convertType = convertType)
  if(!is.null(retval)){
    
    attr(retval, "legacy") <- legacy
    attr(retval, "queryTime") <- Sys.time()
    attr(retval, "url") <- baseURL$url
    
    if(legacy){
      wqp_message()
    } else {
      wqp_message_beta()
      attr(retval, "wqp-request-id") <- attr(retval, "headerInfo")$`wqp-request-id`
    }
  }

  return(retval)
}

#' @name whatWQPmetrics
#' @rdname wqpSpecials
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
  values <- readWQPdots(..., legacy = TRUE)
  
  values <- values[["values"]]

  if ("tz" %in% names(values)) {
    values <- values[!(names(values) %in% "tz")]
  }

  if ("service" %in% names(values)) {
    values <- values[!(names(values) %in% "service")]
  }
  POST = FALSE
  baseURL <- httr2::request(pkg.env[["ActivityMetric"]])
  
  if("siteid" %in% names(values)){
    if(length(values[["siteid"]]) > 1){
      sites <- values[["siteid"]]
      POST = nchar(paste0(sites, collapse = "")) > 2048
      
      baseURL <- get_or_post(baseURL, POST = POST,
                             siteid = sites, 
                             .multi = function(x) paste0(x, collapse = ";"))
      
      values <- values[names(values) != "siteid"]
    }
  }

  baseURL <- get_or_post(baseURL,
                         POST = POST,
                         !!!values,
                         .multi = "explode")
  
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
  
  if(is.null(retval)){
    return(NULL)
  } else {
    wqp_message()
    attr(retval, "queryTime") <- Sys.time()
    attr(retval, "url") <- baseURL$url
  
    return(retval)
  }
}


#' Data Available from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets
#' the data from: <https://www.waterqualitydata.us>.
#' Arguments to the function should be based on
#' <https://www.waterqualitydata.us/webservices_documentation>.
#' The information returned from whatWQPdata describes the
#' available data at the WQP sites, and some metadata on the sites themselves.
#' For example, a row is returned for each individual site that fulfills this 
#' query. In that we can learn how many sampling activities and results 
#' are available for the query. It does not break those results down by any finer 
#' grain. For example, if you ask for "Nutrients" (characteristicGroup), you will
#' not learn what specific nutrients are available at that site. For that 
#' kind of data discovery see `readWQPsummary`.
#'
#' @param \dots see <https://www.waterqualitydata.us/webservices_documentation> for
#' a complete list of options. A list of arguments can also be supplied.
#' One way to figure out how to construct a WQP query is to go to the "Advanced" 
#' form in the Water Quality Portal:
#' <https://www.waterqualitydata.us/#mimeType=csv&providers=NWIS&providers=STORET>
#' Use the form to discover what parameters are available. Once the query is 
#' set in the form, scroll down to the "Query URL". You will see the parameters
#' after "https://www.waterqualitydata.us/#". For example, if you chose "Nutrient"
#' in the Characteristic Group dropdown, you will see characteristicType=Nutrient
#' in the Query URL. The corresponding argument for dataRetrieval is
#' characteristicType = "Nutrient". dataRetrieval users do not need to include
#' mimeType, and providers is optional (these arguments are picked automatically).
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @keywords data import WQP web service
#' @return A data frame that returns basic data availability such as 
#' sites, number of results, and number of sampling activities from the 
#' query parameters for the Water Quality Portal. 
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
#' 
#' bbox <- c(-86.9736, 34.4883, -86.6135, 34.6562)
#' what_bb <- whatWQPdata(bBox = bbox)
#' 
whatWQPdata <- function(..., 
                        convertType = TRUE) {
  values <- readWQPdots(..., legacy = TRUE)
  
  values <- values[["values"]]

  if (any(c("tz", "service", "mimeType") %in% names(values))){
    values <- values[!(names(values) %in% c("tz", "service", "mimeType"))]
  }
  POST = FALSE
  
  baseURL <- httr2::request(pkg.env[["Station"]])
  
  if("siteid" %in% names(values)){
    if(length(values[["siteid"]]) > 1){
      sites <- values[["siteid"]]

      POST = nchar(paste0(sites, collapse = "")) > 2048
        
      baseURL <- get_or_post(baseURL, POST = POST,
                             siteid = sites, 
                             .multi = function(x) paste0(x, collapse = ";"))
      
      values <- values[names(values) != "siteid"]
    }
  }

  baseURL <- get_or_post(req = baseURL, 
                         POST = POST,
                         !!!values,
                         .multi = "explode")
  
  baseURL <- get_or_post(baseURL, 
                         POST = POST,
                         mimeType = "geojson")
  
  # Not sure if there's a geojson option with WQX3
  wqp_message()
  
  doc <- getWebServiceData(baseURL)
  
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
    
    features <- doc[["features"]]
    y <- data.frame(matrix(NA, nrow = length(features), ncol = 15))
    names(y) <- c(names(features[[1]][["properties"]]),
                  "lat", "lon")
    for(i in seq_along(features)){
      single_feature <- features[[i]][["properties"]]
      single_feature[["lat"]] <- unlist(features[[i]][["geometry"]][["coordinates"]][2])
      single_feature[["lon"]] <- unlist(features[[i]][["geometry"]][["coordinates"]][1])
      y[i,] <- single_feature
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
  return(y)
}

get_or_post <- function(req, POST = FALSE, ...){
  
  if(POST){
    req <- httr2::req_body_form(req, ...)

  } else {
    req <- httr2::req_url_query(req, ...)
  }
  return(req)
}

