#' Get USGS Channel Measurements
#' 
#' @description `r get_description("channel-measurements")`
#' 
#' @export
#' @param monitoring_location_id `r get_ogc_params("channel-measurements")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param last_modified `r get_ogc_params("channel-measurements")$last_modified`
#'                                    field_visit_id = NA_character_,
#' @param measurement_number `r get_ogc_params("channel-measurements")$measurement_number`
#' @param field_visit_id `r get_ogc_params("channel-measurements")$field_visit_id`
#' @param time `r get_ogc_params("channel-measurements")$time`
#' @param channel_name `r get_ogc_params("channel-measurements")$channel_name`
#' @param channel_flow `r get_ogc_params("channel-measurements")$channel_flow`
#' @param channel_flow_unit `r get_ogc_params("channel-measurements")$channel_flow_unit`
#' @param channel_width `r get_ogc_params("channel-measurements")$channel_width`
#' @param channel_width_unit `r get_ogc_params("channel-measurements")$channel_width_unit`
#' @param channel_area `r get_ogc_params("channel-measurements")$channel_area`
#' @param channel_area_unit `r get_ogc_params("channel-measurements")$channel_area_unit`
#' @param channel_velocity `r get_ogc_params("channel-measurements")$channel_velocity`
#' @param channel_velocity_unit `r get_ogc_params("channel-measurements")$channel_velocity_unit`
#' @param channel_location_distance `r get_ogc_params("channel-measurements")$channel_location_distance`
#' @param channel_location_distance_unit `r get_ogc_params("channel-measurements")$channel_location_distance_unit`
#' @param channel_stability `r get_ogc_params("channel-measurements")$channel_stability`
#' @param channel_material `r get_ogc_params("channel-measurements")$channel_material`
#' @param channel_evenness `r get_ogc_params("channel-measurements")$channel_evenness`
#' @param horizontal_velocity_description `r get_ogc_params("channel-measurements")$horizontal_velocity_description`
#' @param vertical_velocity_description `r get_ogc_params("channel-measurements")$vertical_velocity_description`
#' @param longitudinal_velocity_description `r get_ogc_params("channel-measurements")$longitudinal_velocity_description`
#' @param measurement_type `r get_ogc_params("channel-measurements")$measurement_type`
#' @param channel_measurement_type `r get_ogc_params("channel-measurements")$channel_measurement_type`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("channel-measurements", "channel_measurements_id")`.
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
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @param no_paging logical, defaults to `FALSE`. If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution. 
#' 
#' @inherit read_waterdata_continuous details
#' 
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' df <- read_waterdata_channel(monitoring_location_id = site)
#' 
#' }
read_waterdata_channel <- function(monitoring_location_id = NA_character_,
                                   field_visit_id = NA_character_,
                                   measurement_number = NA_character_,
                                   time = NA_character_,
                                   channel_name = NA_character_,
                                   channel_flow = NA_character_,
                                   channel_flow_unit = NA_character_,
                                   channel_width = NA_character_,
                                   channel_width_unit = NA_character_,
                                   channel_area = NA_character_,
                                   channel_area_unit = NA_character_,
                                   channel_velocity = NA_character_,
                                   channel_velocity_unit = NA_character_,
                                   channel_location_distance = NA_character_,
                                   channel_location_distance_unit = NA_character_,
                                   channel_stability = NA_character_,
                                   channel_material = NA_character_,
                                   channel_evenness = NA_character_,
                                   horizontal_velocity_description = NA_character_,
                                   vertical_velocity_description = NA_character_,
                                   longitudinal_velocity_description = NA_character_,
                                   measurement_type = NA_character_,
                                   last_modified = NA_character_,
                                   channel_measurement_type = NA_character_,
                                   properties = NA_character_,
                                   skipGeometry = NA,
                                   bbox = NA,
                                   limit = NA,
                                   convertType = TRUE,
                                   no_paging = FALSE){
  
  service <- "channel-measurements"
  output_id <- "channel_measurements_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  return(return_list)
}



