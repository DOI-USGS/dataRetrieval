#' Get USGS Time Series Metadata
#'
#' @description `r get_description("time-series-metadata")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("time-series-metadata")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("time-series-metadata")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param parameter_name `r get_ogc_params("time-series-metadata")$parameter_name`
#' @param statistic_id `r get_ogc_params("time-series-metadata")$statistic_id`
#' Multiple statistic_ids can be requested as a character vector.
#' @param computation_identifier `r get_ogc_params("time-series-metadata")$computation_identifier`
#' Multiple computation_identifiers can be requested as a character vector.
#' @param computation_period_identifier `r get_ogc_params("time-series-metadata")$computation_period_identifier`
#' Multiple computation_period_identifiers can be requested as a character vector.
#' @param sublocation_identifier `r get_ogc_params("time-series-metadata")$sublocation_identifier`
#' @param last_modified `r get_ogc_params("time-series-metadata")$last_modified`
#'
#' See also Details below for more information.
#' @param thresholds `r get_ogc_params("time-series-metadata")$thresholds`
#' @param unit_of_measure `r get_ogc_params("time-series-metadata")$unit_of_measure`
#' @param primary
#' A flag identifying if the time series is a "primary" time series. "Primary" time
#' series (which have this flag) are standard observations which undergo Bureau
#' review and approval processes. Non-primary time series, which will have missing
#' values for "primary", are provisional datasets made available to meet the need
#' for timely best science and to assist with daily operations which need
#' real-time information. Non-primary time series data are only retained by
#' this system for 120 days.
#' @param parent_time_series_id `r get_ogc_params("time-series-metadata")$parent_time_series_id`
#' @param web_description `r get_ogc_params("time-series-metadata")$web_description`
#' @param begin `r get_ogc_params("time-series-metadata")$begin`
#' @param end `r get_ogc_params("time-series-metadata")$end`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("time-series-metadata", "time_series_id")`.
#' The default (`NA`) will return all columns of the data.
#' @param time_series_id `r get_ogc_params("time-series-metadata")$id`
#' @param \dots Not used. Included to help differentiate official Water Data API arguments
#' from more seldom used, optional dataRetrieval-specific arguments.
#' @param begin_utc Deprecated in v1. Use "begin" instead.
#' @param end_utc Deprecated in v1. Use "end" instead.
#' @param state_name Deprecated in v1. Use `read_waterdata_combined_meta` for
#' similar functionality.
#' @param hydrologic_unit_code Deprecated in v1. Use
#' `read_waterdata_combined_meta` for similar functionality.
#' @inheritParams check_arguments_api
#' @inheritParams check_arguments_non_api
#'
#' @inherit read_waterdata_continuous details
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' site <- "USGS-02238500"
#' meta_1 <- read_waterdata_ts_meta(monitoring_location_id = site)
#'
#' meta_multi <- read_waterdata_ts_meta(monitoring_location_id =  c("USGS-01491000",
#'                                                                  "USGS-01645000"),
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
read_waterdata_ts_meta <- function(
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
  parameter_name = NA_character_,
  properties = NA_character_,
  statistic_id = NA_character_,
  last_modified = NA_character_,
  begin_utc = NA_character_,
  end_utc = NA_character_,
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
  bbox = NA,
  begin = NA_character_,
  end = NA_character_,
  ...,
  limit = getOption("dataRetrieval.limit"),
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_meta"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "time-series-metadata"
  output_id <- "time_series_id"
  rlang::check_dots_empty()

  on.exit(options("dataRetrieval.api_version" = "v1"))

  if (!is.na(state_name)) {
    warning(
      "state_name is deprecated starting in v1 of the Water Data APIs.
Use the function read_waterdata_combined_meta instead.
Reverting to v0 version of Water Data APIs."
    )
    options("dataRetrieval.api_version" = "v0")
  }

  if (!is.na(hydrologic_unit_code)) {
    warning(
      "hydrologic_unit_code is deprecated starting in v1 of the Water Data APIs.
Use the function read_waterdata_combined_meta instead.
Reverting to v0 version of Water Data APIs."
    )
    options("dataRetrieval.api_version" = "v0")
  }

  if (!is.na(begin_utc)) {
    warning(
      "begin_utc is deprecated starting in v1 of the Water Data APIs.
Use begin instead.
Reverting to v0 version of Water Data APIs."
    )
    options("dataRetrieval.api_version" = "v0")
  }

  if (!is.na(end_utc)) {
    warning(
      "end_utc is deprecated starting in v1 of the Water Data APIs.
Use end instead.
Reverting to v0 version of Water Data APIs."
    )
    options("dataRetrieval.api_version" = "v0")
  }

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
