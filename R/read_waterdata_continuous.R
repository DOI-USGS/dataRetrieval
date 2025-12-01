#' Get Continuous USGS Water Data
#' 
#' @description `r get_description("continuous")`
#' 
#' Currently, the services only allow 3 years of data to be requested with
#' a single request. If no "time" is specified, the service will return the 
#' last single year of data.
#' 
#' @export
#' @param monitoring_location_id `r get_params("continuous")$monitoring_location_id`
#' @param parameter_code `r get_params("continuous")$parameter_code`
#' @param statistic_id `r get_params("continuous")$statistic_id`
#' @param time `r get_params("continuous")$time`
#' @param value `r get_params("continuous")$value`
#' @param unit_of_measure `r get_params("continuous")$unit_of_measure`
#' @param approval_status `r get_params("continuous")$approval_status`
#' @param last_modified `r get_params("continuous")$last_modified`
#' @param time_series_id `r get_params("continuous")$time_series_id`
#' @param qualifier `r get_params("continuous")$qualifier`
#' @param continuous_id `r get_params("continuous")$id`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r schema <- check_OGC_requests(endpoint = "continuous", type = "schema"); paste(names(schema$properties), collapse = ", ")`
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric 
#' vector structured: c(xmin,ymin,xmax,ymax). Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param max_results The optional maximum number of rows to return. This value
#' must be less than the requested limit. 
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-451605097071701"
#' pcode <- "72019"
#'
#' uv_data_trim <- read_waterdata_continuous(monitoring_location_id = site,
#'                           parameter_code = pcode, 
#'                           properties = c("monitoring_location_id",
#'                                          "value",
#'                                          "time"))
#'
#' uv_data <- read_waterdata_continuous(monitoring_location_id = site,
#'                            parameter_code = pcode,
#'                            time = "P2D")
#' 
#' 
#' # Only return data that has been modified in last 7 days         
#' multi_site2 <- read_waterdata_continuous(monitoring_location_id =  c("USGS-451605097071701",
#'                                                                      "USGS-14181500"),
#'                                                parameter_code = c("00060", "72019"),
#'                                                last_modified = "P7D")
#' 
#' }
read_waterdata_continuous <- function(monitoring_location_id = NA_character_,
                                      parameter_code = NA_character_,
                                      statistic_id = NA_character_,
                                      properties = NA_character_,
                                      time_series_id = NA_character_,
                                      continuous_id = NA_character_,
                                      approval_status = NA_character_,
                                      unit_of_measure = NA_character_,
                                      qualifier = NA_character_,
                                      value = NA,
                                      last_modified = NA_character_,
                                      time = NA_character_,
                                      bbox = NA,
                                      limit = NA,
                                      max_results = NA,
                                      convertType = TRUE){
  
  service <- "continuous"
  output_id <- "continuous_id"
  
  args <- mget(names(formals()))
  args[["skipGeometry"]] <- TRUE

  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  return_list <- return_list[order(return_list$time, return_list$monitoring_location_id), ]
  
  return(return_list)
}



