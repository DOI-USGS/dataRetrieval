#' Get Latest USGS Daily Data
#'
#' @description `r get_description("latest-daily")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("latest-daily")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("latest-daily")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param statistic_id `r get_ogc_params("latest-daily")$statistic_id`
#' Multiple statistic_ids can be requested as a character vector.
#' @param time `r get_ogc_params("latest-daily")$time`
#'
#' See also Details below for more information.
#' @param value `r get_ogc_params("latest-daily")$value`
#' @param unit_of_measure `r get_ogc_params("latest-daily")$unit_of_measure`
#' @param approval_status `r get_ogc_params("latest-daily")$approval_status`
#' @param last_modified `r get_ogc_params("latest-daily")$last_modified`
#'
#' See also Details below for more information.
#' @param time_series_id `r get_ogc_params("latest-daily")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param qualifier `r get_ogc_params("latest-daily")$qualifier`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("latest-daily", "latest_daily_id")`.
#' The default (`NA`) will return all columns of the data.
#'
#' @inheritParams check_arguments_api
#' @inheritParams check_arguments_non_api
#' @inherit read_waterdata_continuous details
#'
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' site <- "USGS-02238500"
#' pcode <- "00060"
#' dv_data_sf <- read_waterdata_latest_daily(monitoring_location_id = site,
#'                               parameter_code = pcode)
#'
#' dv_data_last_modified <- read_waterdata_latest_daily(monitoring_location_id = site,
#'                               parameter_code = pcode,
#'                               last_modified = "P7D")
#'
#' dv_data_trim <- read_waterdata_latest_daily(monitoring_location_id = site,
#'                           parameter_code = pcode,
#'                           properties = c("monitoring_location_id",
#'                                          "value",
#'                                          "time"))
#'
#' dv_data <- read_waterdata_latest_daily(monitoring_location_id = site,
#'                            parameter_code = pcode,
#'                            skipGeometry = TRUE)
#'
#' multi_site <- read_waterdata_latest_daily(monitoring_location_id =  c("USGS-01491000",
#'                                                                       "USGS-01645000"),
#'                               parameter_code = c("00060", "00010"))
#'
#' }
read_waterdata_latest_daily <- function(
  monitoring_location_id = NA_character_,
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
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  limit = getOption("dataRetrieval.limit"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_meta"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "latest-daily"
  output_id <- "latest_daily_id"
  rlang::check_dots_empty()

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
