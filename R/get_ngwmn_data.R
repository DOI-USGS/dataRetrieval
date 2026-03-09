#' Coordinate the request and retrieval of NGWMN calls
#' 
#' @param args arguments from individual functions
#' @param service Endpoint name.
#' 
#' @noRd
#' @return data.frame with attributes
get_ngwmn_data <- function(args,
                           service){

  args <- check_limits(args)
  
  properties <- args[["properties"]]

  args[["service"]] <- service
  
  req <- do.call(construct_ngwmn_requests, args)
  
  no_paging <- grepl("f=csv", req$url)
  
  message("Requesting:\n", req$url)
  
  if(no_paging){
    return_list <- get_csv(req, limit = args[["limit"]])
  } else {
    return_list <- walk_pages(req)
  }
  
  if(!("skipGeometry" %in% names(args)) ||
     is.na(args[["skipGeometry"]])){
    skipGeometry <- FALSE
  } else {
    skipGeometry <- args[["skipGeometry"]]
  }
  
  return_list <- order_results(return_list)
  
  if(getOption("dataRetrieval.attach_request")){
    attr(return_list, "request") <- req
  }
  
  attr(return_list, "queryTime") <- Sys.time()
  return(return_list)
}


#' Create NGWMN API url
#' 
#' Main documentation: <https://ngwmn-qa.wma.chs.usgs.gov/apps/ngwmn/ogcapi/>,
#' Swagger docs: <https://ngwmn-qa.wma.chs.usgs.gov/apps/ngwmn/ogcapi/openapi>.
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
#' req_dv <- construct_ngwmn_requests("daily",
#'                                 monitoring_location_id = sites,
#'                                 parameter_code = c("00060", "00065"),
#'                                 datetime = c(start_date, end_date))
#' 
construct_ngwmn_requests <- function(service,
                                   properties = NA_character_,
                                   bbox = NA,
                                   skipGeometry = FALSE,
                                   no_paging = FALSE,
                                   ...){
  
  POST <- FALSE
  
  single_params <- c("datetime", "last_modified", "datetime", "sample_time",
                     "begin", "end", "time", "limit")
  
  comma_params <- c("monitoring_location_id", 
                    "monitoring_location_obs_number",
                    "properties")
  
  full_list <- list(...)
  
  if(all(is.na(full_list)) & all(is.na(bbox))){
    warning("No filtering arguments specified.")
  }
  # Figure out if the GET request will be > 2048 characters
  # and remove NA's from the comma parameters
  comma_params_filtered <- Filter(Negate(anyNA), lapply(full_list[comma_params], function(x) x[!is.na(x)]))
  comma_params_filtered <- comma_params_filtered[!sapply(comma_params_filtered,is.null)]
  
  single_params_filtered <- Filter(Negate(anyNA), full_list[single_params])
  single_params_filtered <- single_params_filtered[!sapply(single_params_filtered,is.null)]
  
  force_post <- nchar(paste0(unlist(comma_params_filtered), collapse = ",")) > 2048

  if(force_post){
    get_list <- single_params_filtered
  } else {
    # GET list refers to arguments that will go in the URL no matter what (not POST)
    get_list <- c(single_params_filtered, comma_params_filtered)    
  }
  
  if(skipGeometry){
    get_list[["skipGeometry"]] <- skipGeometry
  }
  
  get_list <- get_list[!is.na(get_list)]
  
  time_periods <- c("last_modified", "datetime", "time", "begin", "end", "sample_time")
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
  
  baseURL <- setup_api(service, format = format_type, base = "NGWMN")
  baseURL <- explode_query(baseURL, POST = FALSE, get_list, multi = "comma")
  
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

  #POST list are the arguments that need to be in the POST body
  post_list <- full_list[!names(full_list) %in% names(get_list)]

  post_params <- explode_post(post_list)

  # Should we do a POST?
  POST = length(post_params) > 0

  if(POST){
    baseURL <- baseURL |>
      httr2::req_headers(`Content-Type` = "application/query-cql-json")

    if(length(post_params) > 1){
      post_params <- list(
        "params" = unname(post_params)
      )

      template_path_post <- system.file("templates/post.CQL2", package = "dataRetrieval")
      template_post <- readChar(template_path_post, file.info(template_path_post)$size)

      x <- whisker::whisker.render(template_post, post_params)
    } else {
      x <- post_params[[1]]
    }

    baseURL <- httr2::req_body_raw(baseURL, x)

  } else {
    baseURL <- explode_query(baseURL, POST = FALSE, full_list, multi = "comma")
  }
  
  return(baseURL)
}
