#' Get Latest Continuous USGS Water Data
#' 
#' Description `r get_description("latest-continuous")`
#' 
#' @export
#' @param monitoring_location_id `r get_params("latest-continuous")$monitoring_location_id`
#' @param parameter_code `r get_params("latest-continuous")$parameter_code`
#' @param statistic_id `r get_params("latest-continuous")$statistic_id`
#' @param time `r get_params("latest-continuous")$time`
#' @param value `r get_params("latest-continuous")$value`
#' @param unit_of_measure `r get_params("latest-continuous")$unit_of_measure`
#' @param approval_status `r get_params("latest-continuous")$approval_status`
#' @param last_modified `r get_params("latest-continuous")$last_modified`
#' @param time_series_id `r get_params("latest-continuous")$time_series_id`
#' @param qualifier `r get_params("latest-continuous")$qualifier`
#' @param latest_continuous_id `r get_params("latest-continuous")$id`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r schema <- check_OGC_requests(endpoint = "latest-continuous", type = "schema"); paste(names(schema$properties), collapse = ", ")`
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric 
#' vector structured: c(xmin,ymin,xmax,ymax). Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 10000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param max_results The optional maximum number of rows to return. This value
#' must be less than the requested limit. 
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-451605097071701"
#' pcode <- "72019"
#' uv_data_sf <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                               parameter_code = pcode)
#'
#' uv_data_trim <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                           parameter_code = pcode, 
#'                           properties = c("monitoring_location_id",
#'                                          "value",
#'                                          "time"))
#'
#' uv_data <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                            parameter_code = pcode,
#'                            skipGeometry = TRUE)
#'                         
#' uv_data_period <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                                   parameter_code = pcode,
#'                                   time = "P7D")
#' 
#' multi_site <- read_waterdata_latest_continuous(monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-14181500"),
#'                               parameter_code = c("00060", "72019"))
#' 
#' }
read_waterdata_latest_continuous <- function(monitoring_location_id = NA_character_,
                            parameter_code = NA_character_,
                            statistic_id = NA_character_,
                            properties = NA_character_,
                            time_series_id = NA_character_,
                            latest_continuous_id = NA_character_,
                            approval_status = NA_character_,
                            unit_of_measure = NA_character_,
                            qualifier = NA_character_,
                            value = NA,
                            last_modified = NA_character_,
                            skipGeometry = NA,
                            time = NA_character_,
                            bbox = NA,
                            limit = NA,
                            max_results = NA,
                            convertType = TRUE){
  
  service <- "latest-continuous"
  output_id <- "latest_continuous_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)

  return_list <- return_list[order(return_list$time, return_list$monitoring_location_id), ]
  
  return(return_list)
}



