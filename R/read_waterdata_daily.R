#' Get USGS Daily Data
#'
#' @description `r get_description("daily")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("daily")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("daily")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param statistic_id `r get_ogc_params("daily")$statistic_id`
#' Multiple statistic_ids can be requested as a character vector.
#' @param time `r get_ogc_params("daily")$time`
#'
#' See also Details below for more information.
#' @param value `r get_ogc_params("daily")$value`
#' @param unit_of_measure `r get_ogc_params("daily")$unit_of_measure`
#' @param approval_status `r get_ogc_params("daily")$approval_status`
#' @param last_modified `r get_ogc_params("daily")$last_modified`
#'
#' See also Details below for more information.
#' @param time_series_id `r get_ogc_params("daily")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param qualifier `r get_ogc_params("daily")$qualifier`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("daily", "daily_id")`.
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
#' site <- "USGS-02238500"
#' dv_data_sf <- read_waterdata_daily(monitoring_location_id = site,
#'                               parameter_code = "00060",
#'                               time = c("2021-01-01", "2022-01-01"))
#'
#' dv_data_last_modified <- read_waterdata_daily(monitoring_location_id = site,
#'                               parameter_code = "00060",
#'                               last_modified = "P7D")
#'
#' dv_data_trim <- read_waterdata_daily(monitoring_location_id = site,
#'                           parameter_code = "00060",
#'                           properties = c("value",
#'                                          "time"),
#'                           time = c("2021-01-01", "2022-01-01"))
#'
#' dv_data <- read_waterdata_daily(monitoring_location_id = site,
#'                            parameter_code = "00060",
#'                            skipGeometry = TRUE)
#'
#' dv_data_period <- read_waterdata_daily(monitoring_location_id = site,
#'                                   parameter_code = "00060",
#'                                   time = "P7D")
#'
#' multi_site <- read_waterdata_daily(monitoring_location_id =  c("USGS-01491000",
#'                                                           "USGS-01645000"),
#'                               parameter_code = c("00060", "00010"),
#'                               time = c("2023-01-01", "2024-01-01"))
#'
#' dv_data_quick <- read_waterdata_daily(monitoring_location_id = site,
#'                                    parameter_code = "00060",
#'                                    no_paging = TRUE)
#'
#' dv_post <- read_waterdata_daily(monitoring_location_id = site,
#'                                 approval_status = c("Approved", "Provisional"))
#' # Don't attach "request" attribute:
#' options("dataRetrieval.attach_request" = FALSE)
#' dv_data_no_request <- read_waterdata_daily(monitoring_location_id = site,
#'                               parameter_code = "00060",
#'                               time = c("2021-01-01", "2022-01-01"))
#' }
read_waterdata_daily <- function(
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
  chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
  limit = getOption("dataRetrieval.limit"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "daily"
  output_id <- "daily_id"
  rlang::check_dots_empty()

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
