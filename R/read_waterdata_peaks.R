#' Get USGS Peak Data
#'
#' @description `r get_description("peaks")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("peaks")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("peaks")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param time `r get_ogc_params("peaks")$time`
#'
#' See also Details below for more information.
#' @param value `r get_ogc_params("peaks")$value`
#' @param unit_of_measure `r get_ogc_params("peaks")$unit_of_measure`
#' @param time_series_id `r get_ogc_params("peaks")$time_series_id`
#' @param last_modified `r get_ogc_params("peaks")$last_modified`
#'
#' See also Details below for more information.
#' @param water_year `r get_ogc_params("peaks")$water_year`
#' @param year `r get_ogc_params("peaks")$year`
#' @param month `r get_ogc_params("peaks")$month`
#' @param day `r get_ogc_params("peaks")$day`
#' @param time_of_day `r get_ogc_params("peaks")$time_of_day`
#' @param peak_since `r get_ogc_params("peaks")$peak_since`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("peaks", "peak_id")`.
#' The default (`NA`) will return all columns of the data.
#'
#' @inheritParams check_arguments_api
#' @inheritParams check_arguments_non_api
#'
#' @inherit read_waterdata_continuous details
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' wi_peaks <- read_waterdata_combined_meta(
#'                state_name = "Wisconsin",
#'                data_type = "Peaks",
#'                parameter_code = "00060")
#'
#'
#' dv_data_sf <- read_waterdata_peaks(
#'                monitoring_location_id = wi_peaks$monitoring_location_id[1],
#'                parameter_code = "00060")
#'
#' }
read_waterdata_peaks <- function(
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
  properties = NA_character_,
  time_series_id = NA_character_,
  unit_of_measure = NA_character_,
  value = NA,
  last_modified = NA_character_,
  water_year = NA_character_,
  year = NA_character_,
  month = NA_character_,
  day = NA_character_,
  time_of_day = NA_character_,
  peak_since = NA_character_,
  skipGeometry = NA,
  time = NA_character_,
  bbox = NA,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_meta"),
  limit = getOption("dataRetrieval.limit"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "peaks"
  output_id <- "peak_id"
  rlang::check_dots_empty()

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
