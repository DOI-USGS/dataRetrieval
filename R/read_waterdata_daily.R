#' Get USGS Daily Data
#' 
#' @description `r get_description("daily")`
#' 
#' @export
#' @param monitoring_location_id `r get_params("daily")$monitoring_location_id`
#' @param parameter_code `r get_params("daily")$parameter_code`
#' @param statistic_id `r get_params("daily")$statistic_id`
#' @param time `r get_params("daily")$time`
#' @param value `r get_params("daily")$value`
#' @param unit_of_measure `r get_params("daily")$unit_of_measure`
#' @param approval_status `r get_params("daily")$approval_status`
#' @param last_modified `r get_params("daily")$last_modified`
#' @param time_series_id `r get_params("daily")$time_series_id`
#' @param qualifier `r get_params("daily")$qualifier`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("daily", "daily_id")`.
#' The default (`NA`) will return all columns of the data.
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
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' dv_data_sf <- read_waterdata_daily(monitoring_location_id = site,
#'                               parameter_code = "00060", 
#'                               time = c("2021-01-01", "2022-01-01"))
#'                               
#' dv_data_last_modified <- read_waterdata_daily(monitoring_location_id = site,
#'                               parameter_code = "00060",
#'                               last_modified = "P7D")
#'
#' dv_data_trim <- read_waterdata_daily(monitoring_location_id = site,
#'                           parameter_code = "00060", 
#'                           properties = c("value",
#'                                          "time"),
#'                           time = c("2021-01-01", "2022-01-01"))
#'
#' dv_data <- read_waterdata_daily(monitoring_location_id = site,
#'                            parameter_code = "00060",
#'                            skipGeometry = TRUE)
#'                         
#' dv_data_period <- read_waterdata_daily(monitoring_location_id = site,
#'                                   parameter_code = "00060",
#'                                   time = "P7D")
#' 
#' multi_site <- read_waterdata_daily(monitoring_location_id =  c("USGS-01491000",
#'                                                           "USGS-01645000"),
#'                               parameter_code = c("00060", "00010"),
#'                               limit = 500,
#'                               time = c("2023-01-01", "2024-01-01"))
#' 
#' }
read_waterdata_daily <- function(monitoring_location_id = NA_character_,
                                 parameter_code = NA_character_,
                                 statistic_id = NA_character_,
                                 properties = NA_character_,
                                 time_series_id = NA_character_,
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
  
  service <- "daily"
  output_id <- "daily_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  if(convertType){
    return_list <- order_results(return_list)
    return_list <- move_id_col(return_list, output_id)
  }
  
  return(return_list)
}



