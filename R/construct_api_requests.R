#' Create API url
#' 
#' Main documentation: <https://api.waterdata.usgs.gov/ogcapi/v0/>,
#' Swagger docs: <https://api.waterdata.usgs.gov/ogcapi/v0/openapi?f=html>.
#' 
#' @export
#' @param service Which service available on <https://api.waterdata.usgs.gov/ogcapi/v0/>.
#' @param ... Extra parameters from the specific services.
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth).
#' @param properties The properties that should be included for each feature. The
#' parameter value is a comma-separated list of property names which depend on the
#' service being called.
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @keywords internal
#' @examples
#' site <- "USGS-02238500"
#' pcode <- "00060"
#' req_dv <- construct_api_requests("daily",
#'                                  monitoring_location_id = site,
#'                                  parameter_code = "00060")
#'
#' req_dv <- construct_api_requests("daily",
#'                                  monitoring_location_id = site,
#'                                  parameter_code = c("00060", "00065"))
#' 
#' sites <- c("USGS-01491000", "USGS-01645000")
#' start_date <- "2018-01-01"
#' end_date <- "2022-01-01"
#' req_dv <- construct_api_requests("daily",
#'                                 monitoring_location_id = sites,
#'                                 parameter_code = c("00060", "00065"),
#'                                 datetime = c(start_date, end_date))
#' 
construct_api_requests <- function(service,
                                   properties = NA_character_,
                                   bbox = NA,
                                   skipGeometry = FALSE,
                                   no_paging = FALSE,
                                   ...){
  
  POST <- FALSE
  
  single_params <- c("datetime", "last_modified", 
                     "begin", "end", "time", "limit",
                     "begin_utc", "end_utc")
  comma_params <- c("monitoring_location_id", "parameter_code", 
                    "statistic_id", "time_series_id")
  
  if(service %in% c("monitoring-locations", "parameter-codes", 
                    "time-series-metadata")){
    comma_params <- c(comma_params, "id")
  }
  
  full_list <- list(...)
  
  if(all(is.na(full_list)) & all(is.na(bbox))){
    warning("No filtering arguments specified.")
  }
  
  # GET list refers to arguments that will go in the URL no matter what (not POST)
  get_list <- full_list[names(full_list) %in% c(single_params, comma_params)]

  get_list[["skipGeometry"]] <- skipGeometry
  
  #POST list are the arguments that need to be in the POST body
  post_list <- full_list[!names(full_list) %in% c(single_params, comma_params)]
  
  post_params <- explode_post(post_list)
  
  if(length(post_params) > 0){
    POST = TRUE
  }
  
  get_list <- get_list[!is.na(get_list)]
  
  time_periods <- c("last_modified", "datetime", "time", "begin", "end", "begin_utc", "end_utc")
  if(any(time_periods %in% names(get_list))){

    for(i in time_periods[time_periods %in% names(get_list)]){
      dates <- FALSE
      if (all(service == "daily" & i != "last_modified")){
        dates <- TRUE
      } 
      get_list[[i]] <- format_api_dates(get_list[[i]], date = dates)
      full_list[[i]] <- format_api_dates(full_list[[i]], date = dates) 
    }
  }
  
  format_type <- ifelse(isTRUE(no_paging), "csv", "json")
  
  baseURL <- setup_api(service, format = format_type)
  baseURL <- explode_query(baseURL, POST = FALSE, get_list, multi = "comma")
  
  if(all(!is.na(bbox))){
    baseURL <- httr2::req_url_query(baseURL,
                                    bbox = as.numeric(bbox),
                                    .multi = "comma")      
  }
  
  if(!all(is.na(properties))){
    available_properties <- property_list[[service]]
    
    if(!all(properties %in% available_properties)){
      # Check again:
      schema <- check_OGC_requests(endpoint = service, type = "schema")
      properties_fresh <- names(schema$properties)
      if(!all(properties %in% properties_fresh)){
        stop("Invalid properties: ", 
             paste0(properties[!properties %in% properties_fresh], collapse = ", "))
      }
    }
    
    baseURL <- httr2::req_url_query(baseURL,
                                    properties = properties,
                                    .multi = "comma")    
  }
  
  if(POST){
    baseURL <- baseURL |>
      httr2::req_headers(`Content-Type` = "application/query-cql-json") 
    
    post_params <- list(
      "params" = unname(post_params)
    )
    
    template_path_post <- system.file("templates/post.CQL2", package = "dataRetrieval")
    template_post <- readChar(template_path_post, file.info(template_path_post)$size)
    
    x <- whisker::whisker.render(template_post, post_params)
    baseURL <- httr2::req_body_raw(baseURL, x) 
    
  } else {
    baseURL <- explode_query(baseURL, POST = FALSE, full_list, multi = "comma")
  }
  
  return(baseURL)
}

check_limits <- function(args){
  current_api_limit <- 50000
  
  if(is.na(args[["limit"]])){
    args[["limit"]] <- current_api_limit
  } 
  
  return(args)
}

#' Setup the request for the OGC API requests
#' 
#' @noRd
#' @return httr2 request
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' request <- dataRetrieval:::base_url()
#' request
#' }
base_url <- function(){
  
  httr2::request("https://api.waterdata.usgs.gov/ogcapi/") |> 
    httr2::req_url_path_append(getOption("dataRetrieval")$api_version) 
}

#' Setup the request for a particular endpoint collection
#' 
#' @noRd
#' @return httr2 request
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' request <- dataRetrieval:::setup_api("daily")
#' request
#' }
setup_api <- function(service, format = "json"){
  
  baseURL <- base_url() |> 
    httr2::req_url_path_append("collections") |> 
    httr2::req_url_path_append(service, "items") |> 
    basic_request(format = format) 
  
}

#' Switch endpoint id arg
#' 
#' @noRd
#' @return list
#' @examples
#' 
#' l1 <- list("id" = "1234")
#' dataRetrieval:::switch_arg_id(l1, 
#'                               id_name = "monitoring_location_id",
#'                               service = "monitoring-locations")
#'                               
#' l2 <- list("monitoring_location_id" = "1234")
#' dataRetrieval:::switch_arg_id(l2, 
#'                               id_name = "monitoring_location_id",
#'                               service = "monitoring-locations")
#'                               
#' l3 <- list("monitoring_locations_id" = "1234")
#' dataRetrieval:::switch_arg_id(l3, 
#'                               id_name = "monitoring_location_id",
#'                               service = "monitoring-locations")
#' 
switch_arg_id <- function(ls, id_name, service){

  service_id <- paste0(gsub("-", "_", service), "_id")
  if(!"id" %in% names(ls)){
    if(service_id %in% names(ls)){
      ls[["id"]] <- ls[[service_id]]
    } else {
      ls[["id"]] <- ls[[id_name]]
    }
  }
  
  ls[[service_id]] <- NULL
  ls[[id_name]] <- NULL
  return(ls)
}

#' Format the date request
#' 
#' Users will want to give either start/end dates or 
#' period requests. 
#' 
#' @param datetime character, Date, or POSIX
#' @param format character
#' 
#' @noRd
#' @return character vector with a length of either 1 or 2.
#' @examples
#' 
#' start_end <- c("2021-01-01", "2022-01-01")
#' dataRetrieval:::format_api_dates(start_end, date = TRUE)
#' dataRetrieval:::format_api_dates(start_end, date = FALSE)
#' 
#' start_end <- c("", "")
#' dataRetrieval:::format_api_dates(start_end)
#' 
#' period <- "P7D"
#' dataRetrieval:::format_api_dates(period)
#' 
#' start <- c("2021-01-01", NA)
#' dataRetrieval:::format_api_dates(start)
#' dataRetrieval:::format_api_dates(start, TRUE)
#' 
#' end <- c(NA, "2021-01-01")
#' dataRetrieval:::format_api_dates(end)
#' 
#' end <- c(NA, as.POSIXct("2021-01-01 12:15:00"))
#' dataRetrieval:::format_api_dates(end)
#' 
#' start_end <- as.POSIXct(c("2021-01-01 12:15:00", 
#'                           "2022-01-01 16:45"))
#' dataRetrieval:::format_api_dates(start_end)
#' 
#' start_end <- as.POSIXct(c("2021-01-01 12:15:00", 
#'                           "2022-01-01 16:45"), 
#'                           tz = "America/New_York")
#'                           
#' dataRetrieval:::format_api_dates(start_end)
#' 
#' # If you don't specify a timezone, it will assume UTC
#' start_end2 <- c("2021-01-01 12:15:00", "")
#' dataRetrieval:::format_api_dates(start_end2)
#' 
#' # If you do specify a timezone, it should maintain it, but convert to UTC:
#' start_end2 <- c("2021-01-01T12:15:00-0500", "")
#' dataRetrieval:::format_api_dates(start_end2)
#' 
#' time = c("2014-05-01T00:00:00Z", "2014-05-01T12:00:00Z")
#' dataRetrieval:::format_api_dates(time)
#' 
#' time = c("2014-05-01T00:00Z", "2014-05-01T12:00Z")
#' dataRetrieval:::format_api_dates(time)
format_api_dates <- function(datetime, date = FALSE){
  
  if(is.character(datetime)){
    datetime[datetime == ""] <- NA
  }
  
  if(!any(isTRUE(all(is.na(datetime))) | isTRUE(is.null(datetime)))){
    if(length(datetime) == 1){
      # If the user has "P" or the "/" we assume they know what they are doing
      if(grepl("P", datetime, ignore.case = TRUE) |
         grepl("/", datetime)){
        return(datetime)
      } else {
        datetime1 <- tryCatch({
            lubridate::as_datetime(datetime)
          },
          warning = function(w) {
            strptime(datetime, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
        })
        if(date){
          datetime <- format(datetime1, "%Y-%m-%d")
        } else {
          datetime <- lubridate::format_ISO8601(datetime1, usetz = "Z")
        }
      }
    } else if (length(datetime) == 2) {
      
      datetime1 <- tryCatch({
          lubridate::as_datetime(datetime)
        },
        warning = function(w) {
          strptime(datetime, format = "%Y-%m-%dT%H:%MZ", tz = "UTC")
      })
      
      if(date){
        datetime <- paste0(format(datetime1, "%Y-%m-%d"), collapse = "/")
      } else {
        datetime <- paste0(lubridate::format_ISO8601(datetime1, usetz = "Z"), 
                           collapse = "/")
      }

      datetime <- gsub("NA", "..", datetime)
    } else {
      stop("datetime should only include 1-2 values")
    }
  } else {
    datetime <- NA
  }
  return(datetime)
}

#' Turn request list into POST body cql
#' 
#' @noRd
#' @return character vector of CQL filters
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' 
#' query_list <- list(monitoring_location_id = c("USGS-01491000",
#'                                               "USGS-01645000"),
#'                    parameter_code = c("00060", "00010"))
#' 
#' dataRetrieval:::explode_post(query_list)
#' 
#' }
explode_post <- function(ls){
  
  ls <- Filter(Negate(anyNA), ls)
  params <- NULL
  
  if(length(ls) > 0){
    if(max(lengths(ls)) > 1) {
      
      for(i in seq_along(ls)){
        params[names(ls[i])] <- cql2_param(ls[i])
      }
      
      if(length(params) > 1){
        params[seq_along(1:(length(params)-1))] <- paste0(params[seq_along(1:(length(params)-1))], ",")
      }
    }
  }
  return(params)  
}

#' Create CQL parameters
#' 
#' Helps to give more informative messages on some errors.
#' 
#' @param parameter named vector
#' @noRd
#' @return list
#' @examples
#' 
#' parameter <- list("monitoring_location_id" = c("USGS-02238500",
#'                                                "USGS-01491000"))
#' dataRetrieval:::cql2_param(parameter)
#' 
cql2_param <- function(parameter){
  template_path <- system.file("templates/param.CQL2", package = "dataRetrieval")
  template <- readChar(template_path, file.info(template_path)$size)
  
  parameters <- paste0(unlist(parameter), collapse = '", "')
  parameters <- paste0('"', parameters, '"')
  parameter_list <- list("property" = names(parameter),
                         "parameter" = parameters)
  return(whisker::whisker.render(template, parameter_list))
} 



#' Custom Error Messages
#' 
#' Helps to give more informative messages on some errors.
#' 
#' @param resp httr2 response
#' @return list
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' check_collections <- dataRetrieval:::base_url() |> 
#'   httr2::req_url_path_append("openapi") |> 
#'   httr2::req_url_query(f = "html#/server/getCollections")
#'   
#' collect_request <- dataRetrieval:::basic_request(check_collections)
#' query_ret <- httr2::req_perform(collect_request) 
#' dataRetrieval:::error_body(query_ret)
#' }
#' 
error_body <- function(resp) {
  status <- httr2::resp_status(resp)
  if(status == 429){
    x <- httr2::resp_body_json(resp)$error
    return(x[["message"]])
  } else if (status == 403){
    return("Query request denied. Possible reasons include query exceeding server limits.")
  }
}


#' Basic request to API services
#' 
#' Automatically includes json format, gzip encoding, dataRetrieval
#' user agents, and the X-Api-Key token if available.
#' 
#' @param url_base httr2 request
#' @return list
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' check_collections <- dataRetrieval:::base_url() |> 
#'   httr2::req_url_path_append("openapi") |> 
#'   httr2::req_url_query(f = "html#/server/getCollections")
#' collect_request <- dataRetrieval:::basic_request(check_collections)
#' collect_request
#' }
#' 
basic_request <- function(url_base, format = "json"){
  
  req <- url_base |> 
    httr2::req_user_agent(default_ua()) |> 
    httr2::req_headers(`Accept-Encoding` = c("compress", "gzip")) |> 
    httr2::req_url_query(f = format,
                         lang = "en-US") |> 
    httr2::req_error(body = error_body) |> 
    httr2::req_timeout(seconds = 180)
  
  token <- Sys.getenv("API_USGS_PAT")
  
  if(token != ""){
    req <- req |>
      httr2::req_headers_redacted(`X-Api-Key` = token)
  }
  
  return(req)
  
}


