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
#' @param begin_utc `r get_ogc_params("time-series-metadata")$begin_utc`
#' #'
#' See also Details below for more information.
#' @param end_utc `r get_ogc_params("time-series-metadata")$end_utc`
#'
#' See also Details below for more information.
#' @param hydrologic_unit_code `r get_ogc_params("time-series-metadata")$hydrologic_unit_code`
#' @param state_name `r get_ogc_params("time-series-metadata")$state_name`
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
#'                                            "begin_utc",
#'                                            "end_utc",
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

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
