#' Get USGS Time Series Metadata
#' 
#' Description `r get_description("time-series-metadata")`
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
#' @param thresholds `r get_params("time-series-metadata")$thresholds`
#' @param unit_of_measure `r get_params("time-series-metadata")$unit_of_measure`
#' @param primary `r get_params("time-series-metadata")$primary`
#' @param web_description `r get_params("time-series-metadata")$web_description`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r schema <- check_OGC_requests(endpoint = "time-series-metadata", type = "schema"); paste(names(schema$properties), collapse = ", ")`
#' @param time_series_metadata_id `r get_params("time-series-metadata")$id`
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326.
#' @param limit The optional limit parameter limits the number of items that are
#' presented in the response document. Only items are counted that are on the
#' first level of the collection in the response document. Nested objects
#' contained within the explicitly requested items shall not be counted.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' meta_1 <- read_USGS_ts_meta(monitoring_location_id = site)
#' 
#' meta_multi <- read_USGS_ts_meta(monitoring_location_id =  c("USGS-01491000", 
#'                                                        "USGS-01645000"),
#'                             parameter_code = c("00060", "00010"),
#'                             properties = c("monitoring_location_id",
#'                                            "parameter_code",
#'                                            "begin",
#'                                            "end"),
#'                             skipGeometry = TRUE)
#' }
read_USGS_ts_meta <- function(monitoring_location_id = NA_character_,
                              parameter_code = NA_character_,
                              parameter_name = NA_character_,
                              properties = NA_character_,
                              limit = 10000,
                              bbox = NA,
                              statistic_id = NA_character_,
                              last_modified = NA_character_,
                              begin = NA_character_,
                              end = NA_character_,
                              unit_of_measure = NA_character_,
                              computation_period_identifier = NA_character_,
                              computation_identifier = NA_character_,
                              thresholds = NA,
                              sublocation_identifier = NA_character_,
                              primary = NA_character_,
                              time_series_metadata_id = NA_character_,
                              web_description = NA_character_,
                              skipGeometry = NA,
                              convertType = FALSE){
  
  message("Function in development, use at your own risk.")
  
  service = "time-series-metadata"
  
  args <- mget(names(formals()))
  
  args[["service"]] <-  service
  args[["id"]] <- args[["time_series_id"]]
  args[["time_series_metadata_id"]] <- NULL
  args[["convertType"]] <- NULL
  req_ts_meta <- do.call(construct_api_requests, args)

  return_list <- walk_pages(req_ts_meta)

  if(convertType) return_list <- cleanup_cols(return_list)
  
  return_list <- rejigger_cols(return_list, properties, service)
  
  return(return_list)
  
}
