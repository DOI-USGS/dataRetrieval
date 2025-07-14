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
#' @param limit The optional limit parameter limits the number of items that are
#' presented in the response document. Only items are counted that are on the
#' first level of the collection in the response document. Nested objects
#' contained within the explicitly requested items shall not be counted.
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
                                   limit = NA,
                                   max_results = NA,
                                   skipGeometry = FALSE,
                                   ...){
  
  baseURL <- setup_api(service)
  
  POST <- FALSE
  
  single_params <- c("datetime", "last_modified", "begin", "end", "time")
  
  full_list <- list(...)
  
  if(all(is.na(full_list)) & all(is.na(bbox))){
    warning("No filtering arguments specified.")
  }
  
  get_list <- full_list[names(full_list) %in% single_params]

  get_list[["skipGeometry"]] <- skipGeometry
  
  if(is.na(limit)){
    if(!is.na(max_results)){
      get_list[["limit"]] <- max_results
    } else {
      get_list[["limit"]] <- 10000
    }
  } else {
    if(!is.na(max_results)){
      if(limit > max_results) stop("limit cannot be greater than max_result")
    }
    get_list[["limit"]] <- limit
  }
  
  post_list <- full_list[!names(full_list) %in% single_params]
  
  post_params <- explode_post(post_list)
  
  if(length(post_params) > 0){
    POST = TRUE
  }
  
  time_periods <- c("last_modified", "datetime", "time", "begin", "end")
  if(any(time_periods %in% names(get_list))){
    for(i in time_periods){
      get_list[[i]] <- format_api_dates(get_list[[i]])
      full_list[[i]] <- format_api_dates(full_list[[i]]) 
    }
  }
  
  baseURL <- explode_query(baseURL, POST = FALSE, get_list)
  
  if(all(!is.na(bbox))){
    baseURL <- httr2::req_url_query(baseURL,
                                    bbox = as.numeric(bbox),
                                    .multi = "comma")      
  }
  
  if(!all(is.na(properties))){
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
    baseURL <- explode_query(baseURL, POST = FALSE, full_list)
  }
  
  return(baseURL)
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
setup_api <- function(service){
  
  baseURL <- base_url() |> 
    httr2::req_url_path_append("collections") |> 
    httr2::req_url_path_append(service, "items") |> 
    basic_request() 
  
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

#' Switch properties id
#' 
#' @noRd
#' @return list
#' @examples
#' 
#' properties <- c("id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties, 
#'                               id_name = "monitoring_location_id",
#'                               service = "monitoring-locations")
#'                               
#' properties2 <- c("monitoring_location_id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties2, 
#'                               id_name = "monitoring_location_id",
#'                               service = "monitoring-locations")
#'                               
#' properties3 <- c("monitoring_locations_id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties3, 
#'                               id_name = "monitoring_location_id",
#'                               service = "monitoring-locations")
switch_properties_id <- function(properties, id_name, service){
  
  service_id <- paste0(gsub("-", "_", service), "_id")
  
  last_letter <- substr(service, nchar(service), nchar(service))
  if(last_letter == "s"){
    service_singluar <- substr(service,1, nchar(service)-1)
    service_id_singular <- paste0(gsub("-", "_", service_singluar), "_id")
  } else {
    service_id_singular <- ""
  }
  
  if(!"id" %in% properties){
    if(service_id %in% properties){
      properties[properties == service_id] <- "id"
      
    } else if(service_id_singular %in% properties) {
      properties[properties == service_id_singular] <- "id"
    } else {
      properties[properties == id_name] <- "id"
    }
  }
  
  if(!all(is.na(properties))){

    schema <- check_OGC_requests(endpoint = service,
                                 type = "schema")
    all_properties <- names(schema$properties)
    
    if(all(all_properties[!all_properties %in% c("id", "geometry")] %in% properties)) {
      # Cleans up URL if we're asking for everything
      properties <- NA_character_
    } else {
      properties <- gsub("-", "_", properties)
      properties <- properties[!properties %in% c("id", 
                                                  "geometry",
                                                  paste0(gsub("-", "_", service), "_id"))]
    
    }
    
    if(!all(is.na(properties))){
      match.arg(properties, choices = all_properties,
                several.ok = TRUE)    
    }
  }
  
  return(properties)
}


#' Format the date request
#' 
#' Users will want to give either start/end dates or 
#' period requests. 
#' 
#' 
#' @noRd
#' @return character vector with a length of either 1 or 2.
#' @examples
#' 
#' start_end <- c("2021-01-01", "2022-01-01")
#' dataRetrieval:::format_api_dates(start_end)
#' 
#' start_end <- c("", "")
#' dataRetrieval:::format_api_dates(start_end)
#' 
#' period <- "P7D"
#' dataRetrieval:::format_api_dates(period)
#' 
#' start <- c("2021-01-01", NA)
#' dataRetrieval:::format_api_dates(start)
#' 
#' end <- c(NA, "2021-01-01")
#' dataRetrieval:::format_api_dates(end)
#' 
#' start_end <- as.POSIXct(c("2021-01-01 12:15:00", "2022-01-01 16:45"))
#' dataRetrieval:::format_api_dates(start_end)
#' 
#' start_end2 <- c("2021-01-01 12:15:00", "")
#' dataRetrieval:::format_api_dates(start_end2)
#' 
format_api_dates <- function(datetime){
  
  if(is.character(datetime)){
    datetime[datetime == ""] <- NA
  }
  
  if(!any(isTRUE(all(is.na(datetime))) | isTRUE(is.null(datetime)))){
    if(length(datetime) == 1){
      if(grepl("P", datetime, ignore.case = TRUE) |
         grepl("/", datetime)){
        return(datetime)
      } else {
        datetime <- format(datetime, format = "%Y-%m-%dT%H:%M:%SZ")
      }
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

#' Check OGC requests
#' 
#' @param endpoint Character, can be any existing collection
#' @param type Character, can be "queryables", "schema"
#' @export
#' @keywords internal
#' @return list
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' 
#' dv_queryables <- check_OGC_requests(endpoint = "daily",
#'                                 type = "queryables")
#' dv_schema <- check_OGC_requests(endpoint = "daily",
#'                             type = "schema")
#' ts_meta_queryables <- check_OGC_requests(endpoint = "time-series-metadata",
#'                                 type = "queryables")
#' ts_meta_schema <- check_OGC_requests(endpoint = "time-series-metadata",
#'                                 type = "schema")
#' }
check_OGC_requests <- function(endpoint = "daily",
                               type = "queryables"){
  
  match.arg(type, c("queryables", "schema"))
  
  match.arg(endpoint, pkg.env$api_endpoints)
  
  req <- base_url() |> 
    httr2::req_url_path_append("collections") |> 
    httr2::req_url_path_append(endpoint) |> 
    httr2::req_url_path_append(type) |> 
    basic_request()
  
  query_ret <- req |> 
    httr2::req_perform() |> 
    httr2::resp_body_json() 
  
  return(query_ret)
  
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
basic_request <- function(url_base){
  
  req <- url_base |> 
    httr2::req_user_agent(default_ua()) |> 
    httr2::req_headers(`Accept-Encoding` = c("compress", "gzip")) |> 
    httr2::req_url_query(f = "json",
                         lang = "en-US") |> 
    httr2::req_error(body = error_body) 
  
  token <- Sys.getenv("API_USGS_PAT")
  
  if(token != ""){
    req <- req |>
      httr2::req_headers_redacted(`X-Api-Key` = token)
  }
  
  return(req)
  
}

#' Create service descriptions dynamically
#' 
#' This function populates the parameter descriptions.
#' 
#' @param service Character, can be any of the endpoints
#' @return list
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' ml_desc <- dataRetrieval:::get_description("monitoring-locations")
#' ml_desc
#' }
#' 
get_description <- function(service){

  query_ret <- get_collection() 
  
  tags <- query_ret[["tags"]]
  
  service_index <- which(sapply(tags, function(x){
    x$name == service
  }))
  
  tags[[service_index]][["description"]]
  
}

#' Get collection response
#' 
#' 
#' @return httr2 response
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' collection <- dataRetrieval:::get_collection()
#' collection
#' }
#' 
get_collection <- function(){
  
  check_collections <- base_url() |> 
    httr2::req_url_path_append("openapi") |>
    httr2::req_url_query(f = "html#/server/getCollections")
  
  check_endpoints_req <- basic_request(check_collections)
  
  query_ret <- httr2::req_perform(check_endpoints_req) |> 
    httr2::resp_body_json()
  
  return(query_ret)
}

#' Create parameter descriptions dynamically
#' 
#' This function populates the parameter descriptions.
#' 
#' @param service Character, can be any of the endpoints
#' @return list
#' @noRd
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' ml <- dataRetrieval:::get_params("monitoring-locations")
#' ml$national_aquifer_code
#' }
#' 
get_params <- function(service){
  
  check_queryables_req <- base_url() |> 
    httr2::req_url_path_append("collections") |> 
    httr2::req_url_path_append(service) |> 
    httr2::req_url_path_append("schema") |> 
    basic_request()
  
  query_ret <- httr2::req_perform(check_queryables_req) |> 
    httr2::resp_body_json() 
  
  params <- sapply(query_ret$properties, function(x) x[["description"]]) 

}
