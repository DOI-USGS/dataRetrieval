#' Get USGS Daily Data
#' 
#' Daily data provide one data value to represent water conditions for the day.
#' Throughout much of the history of the USGS, the primary water data available
#' was daily data collected manually at the monitoring location once each day.
#' With improved availability of computer storage and automated transmission of
#' data, the daily data published today are generally a statistical summary or
#' metric of the continuous data collected each day, such as the daily mean,
#' minimum, or maximum value. Daily data are automatically calculated from the
#' continuous data of the same parameter code and are described by parameter code
#' and a statistic code. These data have also been referred to as "daily values"
#' or "DV".
#' 
#' @export
#' @inheritParams construct_dv_requests
#' @param no_sf Boolean, whether or not to return an "sf" object. TRUE returns
#' a basic data frame, FALSE returns a "sf" object.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' pcode <- "00060"
#' dv_data_sf <- read_USGS_dv(monitoring_location_id = site,
#'                         parameter_code = "00060", 
#'                         datetime = c("2021-01-01", "2022-01-01"))
#'                         
#' dv_data <- read_USGS_dv(monitoring_location_id = site,
#'                         parameter_code = "00060",
#'                         no_sf = TRUE)
#' 
#' sites <- c("USGS-01491000", "USGS-01645000")
#' start_date <- "2021-01-01"
#' end_date <- "2022-01-01"
#' #req_dv <- read_USGS_dv(monitoring_location_id =  c("USGS-01491000", "USGS-01645000"),
#' #                        parameter_code = c("00060", "00065"),
#' #                        datetime = c("2021-01-01", "2022-01-01"))
#' 
#' # bbox_data <- read_USGS_dv(bbox = c(-83, 36.5, -81, 38.5),
#' #                           parameter_code = "00060")
#' }
read_USGS_dv <- function(monitoring_location_id = NA_character_,
                         parameter_code = NA_character_,
                         statistic_id = NA_character_,
                         properties = c("monitoring_location_id",
                                        "parameter_code",
                                        "statistic_id",
                                        "time",
                                        "value",
                                        "unit_of_measure",
                                        "approval_status",
                                        "qualifier"),
                         bbox = NA,
                         timeseries_id = NA_character_,
                         id = NA_character_,
                         approval_status = NA_character_,
                         unit_of_measure = NA_character_,
                         qualifier = NA_character_,
                         value = NA,
                         last_modified = NA_character_,
                         limit = 10000,
                         crs = NA_character_,
                         bbox_crs = NA_character_,
                         skipGeometry = NA,
                         offset = NA,
                         datetime = NA_character_,
                         filter = NA_character_,
                         no_sf = FALSE){
  
  message("Function in development, use at your own risk.")
  
  use_sf <- all(pkg.env$local_sf, !no_sf)
  page <- 1
  
  if(!use_sf){
    skipGeometry <- TRUE
  }
  
  dv_req <- construct_dv_requests(monitoring_location_id = monitoring_location_id,
                                  parameter_code = parameter_code,
                                  statistic_id = statistic_id,
                                  properties = properties,
                                  bbox = bbox,
                                  timeseries_id = timeseries_id,
                                  id = id,
                                  approval_status = approval_status,
                                  unit_of_measure = unit_of_measure,
                                  qualifier = qualifier,
                                  value = value,
                                  last_modified = last_modified,
                                  limit = limit,
                                  crs = crs,
                                  bbox_crs = bbox_crs,
                                  skipGeometry = skipGeometry,
                                  offset = offset,
                                  datetime = datetime,
                                  filter = filter)
  
  return_list <- walk_pages(dv_req, use_sf)
  
  return_list <- return_list[order(return_list$time, return_list$monitoring_location_id), ]
  
  return(return_list)
}

cleanup_cols <- function(df){
  
  if("qualifier" %in% names(df)){
    df$qualifier <- vapply(X = df$qualifier, 
                           FUN = function(x) paste(x, collapse = ","),
                           FUN.VALUE =  c(NA_character_))
  }
  
  if("time" %in% names(df)){
    df$time <- as.Date(df$time)
  }
  
  if("value" %in% names(df)){
    df$value <- as.numeric(df$value)
  }
  df
}

walk_pages <- function(req, use_sf) {
  
  walk_pages_recursive(
    req = req,
    page = 1,
    contents = list(),
    use_sf
  )
}

# Change to https://httr2.r-lib.org/reference/req_perform_iterative.html
walk_pages_recursive <- function(req, page, contents, use_sf) {
  
  message("GET: ", req$url) 

  returned_contents <- httr2::req_perform(req)
  
  if(httr2::resp_status(returned_contents) != 200){
    if(httr2::resp_status(returned_contents) == 429){
      stop("You hit your hourly limit for requests. To increase the limit, 
           see: https://api.waterdata.usgs.gov/docs/ogcapi/keys/")
    } 
  }
  
  header_info <- httr2::resp_headers(returned_contents)
  message("Remaining requests this hour:", header_info$`x-ratelimit-remaining`)
  
  contents[[page]] <- returned_contents |> 
    httr2::resp_body_string() 
  
  json_content <- jsonlite::fromJSON(contents[[page]] )

  next_page <- json_content$links[, "rel", drop = TRUE] == "next"
  
  if (!any(next_page)) {
    
    if(use_sf){
      return_df <- lapply(contents, function(x) {
          df_i <- sf::read_sf(x) |> 
            cleanup_cols()
          df_i
        }) |> 
        do.call(what = rbind)
    } else {
      return_df <- lapply(contents, function(x) {
          df_i <- jsonlite::fromJSON(x)[["features"]][["properties"]] |> 
            cleanup_cols()
          df_i
        }) |> 
        do.call(what = rbind)
    }
    
    return(return_df)
    
  } else {
    
    make_request <- httr2::request(json_content$links[, "href", drop = TRUE][next_page]) |> 
      httr2::req_user_agent(default_ua())
    
    if( Sys.getenv("API_USGS_PAT") != ""){
      make_request <- make_request |> 
        httr2::req_headers(`Accept-Encoding` = c("compress", "gzip"),
                           `X-Api-Key` = Sys.getenv("API_USGS_PAT"))
    } else {
      make_request <- make_request |> 
        httr2::req_headers(`Accept-Encoding` = c("compress", "gzip"))     
    }
    
    Tailcall(
      walk_pages_recursive,
      make_request,
      page + 1,
      contents,
      use_sf
    )
  }
}

#' Create DV url
#' 
#' Main documentation: \url{https://api.waterdata.usgs.gov/ogcapi/v0/},
#' Swagger docs: \url{https://api.waterdata.usgs.gov/ogcapi/v0/openapi?f=html}.
#' 
#' @export
#' @param monitoring_location_id A unique identifier representing a single monitoring
#' location. This corresponds to the id field in the sites endpoint. Monitoring
#' location IDs are created by combining the agency code of the agency responsible
#' for the monitoring location (e.g. USGS) with the ID number of the monitoring
#' location (e.g. 02238500), separated by a hyphen (e.g. USGS-02238500).
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth).
#' @param crs Indicates the coordinate reference system for the results.
#' @param bbox_crs Indicates the coordinate reference system for the given bbox
#' coordinates.
#' @param properties The properties that should be included for each feature. The
#' parameter value is a comma-separated list of property names. Available values:
#' id, timeseries_id, monitoring_location_id, parameter_code, statistic_id, time,
#' value, unit_of_measure, approval_status, qualifier, last_modified.
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature.
#' @param offset The optional offset parameter indicates the index within the
#' result set from which the server shall begin presenting results in the response
#' document. The first element has an index of 0 (default).
#' @param datetime Either a date-time or an interval. Only features that have a
#' temporal property that intersects the value of datetime are selected. If a 
#' feature has multiple temporal properties, it is the decision of the server
#' whether only a single temporal property is used to determine the extent or
#' all relevant temporal properties.
#' @param id A universally unique identifier (UUID) representing a single version
#' of a record. It is not stable over time. Every time the record is refreshed in
#' our database (which may happen as part of normal operations and does not imply
#' any change to the data itself) a new ID will be generated. To uniquely identify
#' a single observation over time, compare the time and timeseries_id fields; each
#' timeseries will only have a single observation at a given time.
#' @param timeseries_id A unique identifier representing a single timeseries.
#' This corresponds to the id field in the timeseries-metadata endpoint.
#' @param parameter_code Parameter codes are 5-digit codes used to identify the
#' constituent measured and the units of measure. 
#' @param statistic_id A code corresponding to the statistic an observation represents.
#' Example codes include 00001 (max), 00002 (min), and 00003 (mean). 
#' @param time The date an observation represents. 
#' @param value The value of the observation. Values are transmitted as strings
#' in the JSON response format in order to preserve precision.
#' @param unit_of_measure A human-readable description of the units of measurement
#' associated with an observation.
#' @param approval_status Some of the data that you have obtained from this U.S.
#' Geological Survey database may not have received Director's approval. Any such
#' data values are qualified as provisional and are subject to revision. Provisional
#' data are released on the condition that neither the USGS nor the United States
#' Government may be held liable for any damages resulting from its use. This field
#' reflects the approval status of each record, and is either "Approved", meaning
#' processing review has been completed and the data is approved for publication,
#' or "Provisional" and subject to revision. 
#' @param qualifier This field indicates any qualifiers associated with an observation,
#' for instance if a sensor may have been impacted by ice or if values were estimated.
#' @param last_modified The last time a record was refreshed in our database. This
#' may happen due to regular operational processes and does not necessarily indicate
#' anything about the measurement has changed. You can query this field using
#' date-times or intervals.
#' @param limit The optional limit parameter limits the number of items that are
#' presented in the response document. Only items are counted that are on the
#' first level of the collection in the response document. Nested objects
#' contained within the explicitly requested items shall not be counted.
#' @examples
#' site <- "USGS-02238500"
#' pcode <- "00060"
#' req_dv <- construct_dv_requests(monitoring_location_id = site,
#'                                 parameter_code = "00060")
#'
#' req_dv <- construct_dv_requests(monitoring_location_id = site,
#'                                 parameter_code = c("00060", "00065"))
#' 
#' sites <- c("USGS-01491000", "USGS-01645000")
#' start_date <- "2018-01-01"
#' end_date <- "2022-01-01"
#' req_dv <- construct_dv_requests(monitoring_location_id = sites,
#'                                 parameter_code = c("00060", "00065"),
#'                                 datetime = c(start_date, end_date))
#' 
construct_dv_requests <- function(monitoring_location_id = NA_character_,
                                  parameter_code = NA_character_,
                                  statistic_id = NA_character_,
                                  properties = c("monitoring_location_id",
                                                 "parameter_code",
                                                 "statistic_id",
                                                 "time",
                                                 "value",
                                                 "unit_of_measure",
                                                 "approval_status",
                                                 "qualifier"),
                                  bbox = NA,
                                  timeseries_id = NA_character_,
                                  id = NA_character_,
                                  approval_status = NA_character_,
                                  unit_of_measure = NA_character_,
                                  qualifier = NA_character_,
                                  value = NA,
                                  last_modified = NA_character_,
                                  limit = 10000,
                                  crs = NA_character_,
                                  bbox_crs = NA_character_,
                                  skipGeometry = FALSE,
                                  offset = NA,
                                  datetime = NA_character_
                                  ){
  
  match.arg(properties, choices = c("id",
                                    "timeseries_id",
                                    "monitoring_location_id",
                                    "parameter_code",
                                    "statistic_id",
                                    "time",
                                    "value",
                                    "unit_of_measure",
                                    "approval_status",
                                    "qualifier",
                                    "last_modified", NA_character_),
            several.ok = TRUE)

  baseURL <- httr2::request("https://api.waterdata.usgs.gov/ogcapi/v0/collections") |> 
    httr2::req_url_path_append("daily", "items") |> 
    httr2::req_user_agent(default_ua())
  
  token <- Sys.getenv("API_USGS_PAT")
  filter <- NA_character_
  
  ###########################################################
  # This is all going to be done eventually with a POST CQL request:
  if(isTRUE(length(monitoring_location_id) > 1)){
    filter <- c(filter[!is.na(filter)], 
                paste0("monitoring_location_id IN ('", 
                     paste0(monitoring_location_id, 
                            collapse = "', '"), "')"))
    monitoring_location_id <- NA_character_
  }
  
  if(isTRUE(length(parameter_code) > 1)){
    filter <- c(filter[!is.na(filter)], 
                paste0("parameter_code IN ('", 
                     paste0(parameter_code, 
                            collapse = "', '"), "')"))
    parameter_code <- NA_character_
  }
  ###########################################################
  
  if(!any(is.na(datetime))){
    if(length(datetime) == 1){
      datetime <- format(datetime, format = "%Y-%m-%dT%H:%M:%SZ")
    } else if (length(datetime) == 2) {
      datetime <- as.POSIXct(datetime)
      datetime <- paste0(vapply(datetime, FUN =  function(x) {
        format(x, format = "%Y-%m-%dT%H:%M:%SZ")},
        FUN.VALUE =  c(NA_character_)
      ), collapse = "/")
      datetime <- gsub("NA", "..", datetime)
    } else {
      stop("datetime should only include 1-2 values")
    }
  }
  
  POST <- FALSE
  
  baseURL <- explode_query(baseURL, POST = POST,
                           list(monitoring_location_id = monitoring_location_id,
                                parameter_code = parameter_code,
                                statistic_id = statistic_id,
                                timeseries_id = timeseries_id,
                                id = id,
                                approval_status = approval_status,
                                unit_of_measure = unit_of_measure,
                                qualifier = qualifier,
                                value = value,
                                last_modified = last_modified,
                                limit = limit,
                                crs = crs,
                                bbox_crs = bbox_crs,
                                skipGeometry = skipGeometry,
                                offset = offset,
                                datetime = datetime,
                                filter = filter,
                                f = "json",
                                lang = "en-US"))

  if(all(!is.na(bbox))){
    baseURL <- httr2::req_url_query(baseURL,
                                    bbox = bbox,
                                    .multi = "comma")      
  }
  
  if(POST){
    baseURL <- httr2::req_body_json(baseURL,
                                    properties = properties)    
  } else {
    baseURL <- httr2::req_url_query(baseURL,
                                    properties = properties,
                                    .multi = "comma")
  }

  
  if(token != ""){
    baseURL <- baseURL |>
      httr2::req_headers(`X-Api-Key` = token,
                         `Accept-Encoding` = c("compress", "gzip"))
  } else {
    baseURL <- baseURL |>
      httr2::req_headers(`Accept-Encoding` = c("compress", "gzip"))    
  }

  return(baseURL)
}


#' Check OGC requests
#' 
#' @param endpoint Character, can be "daily", "timeseries-metadata"
#' @param type Character, can be "queryables", "schema"
#' @export
#' @return list
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' 
#' dv_queryables <- check_OGC_requests(endpoint = "daily",
#'                                 type = "queryables")
#' dv_schema <- check_OGC_requests(endpoint = "daily",
#'                             type = "schema")
#' ts_meta_queryables <- check_OGC_requests(endpoint = "timeseries-metadata",
#'                                 type = "queryables")
#' ts_meta_schema <- check_OGC_requests(endpoint = "timeseries-metadata",
#'                                 type = "schema")
#' }
check_OGC_requests <- function(endpoint = "daily",
                           type = "queryables"){
  
  match.arg(endpoint, c("daily", "timeseries-metadata"))
  match.arg(type, c("queryables", "schema"))
  
  check_req <- httr2::request("https://api.waterdata.usgs.gov/ogcapi/v0/collections") |> 
    httr2::req_url_path_append(endpoint) |> 
    httr2::req_url_path_append(type) |> 
    httr2::req_user_agent(default_ua()) |> 
    httr2::req_url_query(f = "json",
                         lang = "en-US") 
  
  message("GET: ", check_req$url) 
  
  query_ret <- httr2::req_perform(check_req) |> 
    httr2::resp_body_json() 
  
  return(query_ret)
  
}