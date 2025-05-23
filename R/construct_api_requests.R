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
                                   limit = 10000,
                                   skipGeometry = FALSE,
                                   ...){
  
  schema <- check_OGC_requests(endpoint = service,
                               type = "schema")
  all_properties <- names(schema$properties)
  
  match.arg(properties, choices = c(all_properties, NA_character_),
            several.ok = TRUE)
  
  use_sf <- all(pkg.env$local_sf)
  
  if(!use_sf){
    skipGeometry <- TRUE
  }
  
  if(all(all_properties[!all_properties %in% c("id", "geometry")] %in% properties)) {
    # Cleans up URL if we're asking for everything
    properties <- NA_character_
  } else {
    if(all(!is.na(properties))){
      properties <- gsub("-", "_", properties)
      properties <- properties[!properties %in% c("id", 
                                                  "geometry",
                                                  paste0(gsub("-", "_", service), "_id"))]
    }
  }
  
  baseURL <- setup_api(service)
  
  POST <- FALSE
  
  template_path_post <- system.file("templates/post.CQL2", package = "dataRetrieval")
  template_post <- readChar(template_path_post, file.info(template_path_post)$size)
  
  single_params <- c("datetime", "last_modified", "begin", "end", "time")
  
  full_list <- list(...)
  
  if(all(is.na(full_list)) & all(is.na(bbox))){
    warning("No filtering arguments specified.")
  }
  
  get_list <- full_list[names(full_list) %in% single_params]

  get_list[["skipGeometry"]] <- skipGeometry
  get_list[["limit"]] <- limit
  
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
                                    bbox = bbox,
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
    
    x <- whisker::whisker.render(template_post, post_params)
    baseURL <- httr2::req_body_raw(baseURL, x) 
    
  } else {
    baseURL <- explode_query(baseURL, POST = FALSE, full_list)
  }
  
  return(baseURL)
}

setup_api <- function(service){
  
  baseURL <- httr2::request("https://api.waterdata.usgs.gov/ogcapi/v0/collections") |> 
    httr2::req_url_path_append(service, "items") |> 
    basic_request() 
  
}

format_api_dates <- function(datetime){
  
  if(!any(isTRUE(is.na(datetime)) | isTRUE(is.null(datetime)))){
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
  }
  return(datetime)
}

explode_post <- function(ls){
  
  ls <- Filter(Negate(anyNA), ls)
  params <- NULL
  
  if(max(lengths(ls)) > 1) {
    
    for(i in seq_along(ls)){
      params[names(ls[i])] <- cql2_param(ls[i])
    }
    
    if(length(params) > 1){
      params[seq_along(1:(length(params)-1))] <- paste0(params[seq_along(1:(length(params)-1))], ",")
    }
  }
  return(params)  
}

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
  
  check_collections <- httr2::request("https://api.waterdata.usgs.gov/ogcapi/v0/openapi?f=html#/server/getCollections")
  
  check_endpoints_req <- basic_request(check_collections)
  query_ret <- httr2::req_perform(check_endpoints_req) |> 
    httr2::resp_body_json() 
  
  services <- sapply(query_ret$tags, function(x) x[["name"]])
  
  match.arg(endpoint, services)
  
  url_base <- httr2::request("https://api.waterdata.usgs.gov/ogcapi/v0/collections") |> 
    httr2::req_url_path_append(endpoint) |> 
    httr2::req_url_path_append(type) 
  
  req <- basic_request(url_base)
  
  query_ret <- httr2::req_perform(req) |> 
    httr2::resp_body_json() 
  
  return(query_ret)
  
}

basic_request <- function(url_base){
  
  req <- url_base |> 
    httr2::req_user_agent(default_ua()) |> 
    httr2::req_headers(`Accept-Encoding` = c("compress", "gzip")) |> 
    httr2::req_url_query(f = "json",
                         lang = "en-US") 
  
  token <- Sys.getenv("API_USGS_PAT")
  
  if(token != ""){
    req <- req |>
      httr2::req_headers_redacted(`X-Api-Key` = token)
  }
  
  return(req)
  
}

# Create descriptions dynamically
get_description <- function(service){
  
  check_collections <- httr2::request("https://api.waterdata.usgs.gov/ogcapi/v0/openapi?f=html#/server/getCollections")
  
  check_endpoints_req <- basic_request(check_collections)
  query_ret <- httr2::req_perform(check_endpoints_req) |> 
    httr2::resp_body_json() 
  tags <- query_ret[["tags"]]
  
  service_index <- which(sapply(tags, function(x){
    x$name == service
  }))
  
  tags[[service_index]][["description"]]
  
}

# Create parameter descriptions dynamically
get_params <- function(service){
  
  check_queryables_req <- httr2::request("https://api.waterdata.usgs.gov/ogcapi/v0/collections") |> 
    httr2::req_url_path_append(service) |> 
    httr2::req_url_path_append("schema")
  
  check_queryables_req <- basic_request(check_queryables_req)
  query_ret <- httr2::req_perform(check_queryables_req) |> 
    httr2::resp_body_json() 
  params <- sapply(query_ret$properties, function(x) x[["description"]]) 

}

