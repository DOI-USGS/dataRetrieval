#' Get USGS Field Measurement Metadata
#'
#' @description `r get_description("field-measurements-metadata")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("field-measurements-metadata")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("field-measurements-metadata")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param parameter_name `r get_ogc_params("field-measurements-metadata")$parameter_name`
#' Multiple parameter_names can be requested as a character vector.
#' @param parameter_description `r get_ogc_params("field-measurements-metadata")$parameter_description`
#' Multiple parameter_descriptions can be requested as a character vector.
#' @param last_modified `r get_ogc_params("field-measurements-metadata")$last_modified`
#'
#' See also Details below for more information.
#' @param begin `r get_ogc_params("field-measurements-metadata")$begin`
#'
#' See also Details below for more information.
#' @param end `r get_ogc_params("field-measurements-metadata")$end`
#'
#' See also Details below for more information.
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("field-measurements-metadata", "field_measurement_id")`.
#' The default (`NA`) will return all columns of the data.
#'
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
#' field_data_sf <- read_waterdata_field_meta(monitoring_location_id = site)
#'
#' groundwater <- read_waterdata_field_meta(monitoring_location_id = "USGS-375907091432201")
#'
#' gwl_data <- read_waterdata_field_meta(monitoring_location_id = "USGS-02238500",
#'                            parameter_code = "00060",
#'                            begin = as.POSIXct(c(NA,
#'                                                "2025-08-27 12:00:00"),
#'                                              tz = "America/Chicago"),
#'                            skipGeometry = TRUE)
#'
#' gwl_data_period <- read_waterdata_field_meta(
#'                                   monitoring_location_id = "USGS-375907091432201",
#'                                   parameter_code = "72019",
#'                                   last_modified = "P1Y")
#'
#' multi_site <- read_waterdata_field_meta(
#'                               monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-263819081585801"),
#'                               parameter_code = c("62611", "72019"))
#'
#'
#' surface_water <- read_waterdata_field_meta(
#'                          monitoring_location_id = c("USGS-07069000",
#'                                                     "USGS-07064000",
#'                                                     "USGS-07068000"),
#'                          end = "2024-07-01T00:00:00Z/..",
#'                          parameter_code = "00060")
#'
#'
#' }
read_waterdata_field_meta <- function(
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
  parameter_name = NA_character_,
  parameter_description = NA_character_,
  begin = NA_character_,
  end = NA_character_,
  last_modified = NA_character_,
  properties = NA_character_,
  skipGeometry = NA,
  bbox = NA,
  limit = NA,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_meta"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "field-measurements-metadata"
  output_id <- "field_series_id"
  rlang::check_dots_empty()

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
