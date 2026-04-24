#' Get USGS Field Measurement Water Data
#'
#' @description `r get_description("field-measurements")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("field-measurements")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("field-measurements")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param observing_procedure_code `r get_ogc_params("field-measurements")$observing_procedure_code`
#' @param observing_procedure `r get_ogc_params("field-measurements")$observing_procedure`
#' @param time `r get_ogc_params("field-measurements")$time`
#'
#' See also Details below for more information.
#' @param value `r get_ogc_params("field-measurements")$value`
#' @param unit_of_measure `r get_ogc_params("field-measurements")$unit_of_measure`
#' @param approval_status `r get_ogc_params("field-measurements")$approval_status`
#' @param last_modified `r get_ogc_params("field-measurements")$last_modified`
#'
#' See also Details below for more information.
#' @param qualifier `r get_ogc_params("field-measurements")$qualifier`
#' @param field_visit_id `r get_ogc_params("field-measurements")$field_visit_id`
#' @param field_measurements_series_id `r get_ogc_params("field-measurements")$field_measurements_series_id`
#' @param vertical_datum `r get_ogc_params("field-measurements")$vertical_datum`
#' @param measuring_agency `r get_ogc_params("field-measurements")$measuring_agency`
#' @param control_condition `r get_ogc_params("field-measurements")$control_condition`
#' What and where the control of flow is for the gage pool.
#' @param measurement_rated  `r get_ogc_params("field-measurements")$measurement_rated`
#' Rated measurement based on the hydrologic/hydraulic conditions in which the measurement was made
#' (excellent (2 percent), good (5 percent), fair (8 percent), or poor (more than 8 percent).                                                        percent)
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("field-measurements", "field_measurement_id")`.
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
#' field_data_sf <- read_waterdata_field_measurements(monitoring_location_id = site)
#'
#' groundwater <- read_waterdata_field_measurements(monitoring_location_id = "USGS-375907091432201")
#'
#' field_data <- read_waterdata_field_measurements(monitoring_location_id = "USGS-02238500",
#'                            parameter_code = "00060",
#'                            time = as.POSIXct(c("2024-02-26 15:00:00",
#'                                                "2025-08-27 12:00:00"),
#'                                              tz = "America/Chicago"),
#'                            skipGeometry = TRUE)
#'
#' gwl_data_period <- read_waterdata_field_measurements(
#'                                   monitoring_location_id = "USGS-375907091432201",
#'                                   parameter_code = "72019",
#'                                   time = "P20Y")
#'
#' multi_site <- read_waterdata_field_measurements(
#'                               monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-263819081585801"),
#'                               parameter_code = c("62611", "72019"))
#'
#' old_df <- read_waterdata_field_measurements(monitoring_location_id = "USGS-425957088141001",
#'                                               time = c("1980-01-01", NA))
#'
#' new_df <- read_waterdata_field_measurements(monitoring_location_id = "USGS-425957088141001",
#'                                               time = c(NA, "2020-01-01"))
#'
#' surface_water <- read_waterdata_field_measurements(
#'                          monitoring_location_id = c("USGS-07069000",
#'                                                     "USGS-07064000",
#'                                                     "USGS-07068000"),
#'                          time = "2024-07-01T00:00:00Z/..",
#'                          parameter_code = "00060")
#'
#'
#' }
read_waterdata_field_measurements <- function(
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
  observing_procedure_code = NA_character_,
  properties = NA_character_,
  field_visit_id = NA_character_,
  field_measurements_series_id = NA_character_,
  approval_status = NA_character_,
  unit_of_measure = NA_character_,
  qualifier = NA_character_,
  value = NA,
  last_modified = NA_character_,
  observing_procedure = NA_character_,
  vertical_datum = NA_character_,
  measuring_agency = NA_character_,
  control_condition = NA_character_,
  measurement_rated = NA_character_,
  skipGeometry = NA,
  time = NA_character_,
  bbox = NA,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  limit = getOption("dataRetrieval.limit"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "field-measurements"
  output_id <- "field_measurement_id"
  rlang::check_dots_empty()

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
