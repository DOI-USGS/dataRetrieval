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
#' @param channel_location_direction `r get_ogc_params("channel-measurements")$channel_location_direction`
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
#' @inheritParams check_arguments_api
#' @inheritParams check_arguments_non_api
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
read_waterdata_channel <- function(
  monitoring_location_id = NA_character_,
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
  channel_location_direction = NA_character_,
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
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  limit = getOption("dataRetrieval.limit"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "channel-measurements"
  output_id <- "channel_measurements_id"

  rlang::check_dots_empty()
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
