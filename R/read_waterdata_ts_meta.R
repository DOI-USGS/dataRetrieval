#' Get USGS Time Series Metadata
#' 
#' @description `r get_description("time-series-metadata")`
#' 
#' @export
#' @param monitoring_location_id `r get_params("time-series-metadata")$monitoring_location_id`
#' @param parameter_code `r get_params("time-series-metadata")$parameter_code`
#' @param parameter_name `r get_params("time-series-metadata")$parameter_name`
#' @param statistic_id `r get_params("time-series-metadata")$statistic_id`
#' @param computation_identifier `r get_params("time-series-metadata")$computation_identifier`
#' @param computation_period_identifier `r get_params("time-series-metadata")$computation_period_identifier`
#' @param sublocation_identifier `r get_params("time-series-metadata")$sublocation_identifier`
#' @param last_modified `r get_params("time-series-metadata")$last_modified`
#' @param begin `r get_params("time-series-metadata")$begin`
#' @param end `r get_params("time-series-metadata")$end`
#' @param hydrologic_unit_code `r get_params("time-series-metadata")$hydrologic_unit_code`
#' @param state_name `r get_params("time-series-metadata")$state_name`
#' @param thresholds `r get_params("time-series-metadata")$thresholds`
#' @param unit_of_measure `r get_params("time-series-metadata")$unit_of_measure`
#' @param primary `r get_params("time-series-metadata")$primary`
#' @param parent_time_series_id `r get_params("time-series-metadata")$parent_time_series_id`
#' @param web_description `r get_params("time-series-metadata")$web_description`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("time-series-metadata", "time_series_id")`.
#' The default (`NA`) will return all columns of the data.
#' @param time_series_id `r get_params("time-series-metadata")$id`
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
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param no_paging logical, defaults to `FALSE`. If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution. 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' meta_1 <- read_waterdata_ts_meta(monitoring_location_id = site)
#' 
#' meta_multi <- read_waterdata_ts_meta(monitoring_location_id =  c("USGS-01491000", 
#'                                                             "USGS-01645000"),
#'                             parameter_code = c("00060", "00010"),
#'                             properties = c("monitoring_location_id",
#'                                            "parameter_code",
#'                                            "begin",
#'                                            "end",
#'                                            "time_series_id"),
#'                             skipGeometry = TRUE)
#'                             
#' meta_wi <- read_waterdata_ts_meta(state_name = "Wisconsin")
#' }
read_waterdata_ts_meta <- function(monitoring_location_id = NA_character_,
                              parameter_code = NA_character_,
                              parameter_name = NA_character_,
                              properties = NA_character_,
                              statistic_id = NA_character_,
                              last_modified = NA_character_,
                              begin = NA_character_,
                              end = NA_character_,
                              hydrologic_unit_code = NA_character_,
                              state_name = NA_character_,
                              unit_of_measure = NA_character_,
                              computation_period_identifier = NA_character_,
                              computation_identifier = NA_character_,
                              thresholds = NA,
                              sublocation_identifier = NA_character_,
                              primary = NA_character_,
                              parent_time_series_id = NA_character_,
                              time_series_id = NA_character_,
                              web_description = NA_character_,
                              skipGeometry = NA,
                              limit = NA,
                              max_results = NA,
                              bbox = NA,
                              convertType = FALSE,
                              no_paging = FALSE){

  service = "time-series-metadata"
  output_id <- "time_series_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  return(return_list)
  
}
