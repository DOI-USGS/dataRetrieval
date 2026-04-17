#' Get Latest Continuous USGS Water Data
#'
#' @description `r get_description("latest-continuous")`
#'
#' @export
#' @param monitoring_location_id `r get_ogc_params("latest-continuous")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("latest-continuous")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param time `r get_ogc_params("latest-continuous")$time`
#'
#' See also Details below for more information.
#' @param value `r get_ogc_params("latest-continuous")$value`
#' @param unit_of_measure `r get_ogc_params("latest-continuous")$unit_of_measure`
#' @param approval_status `r get_ogc_params("latest-continuous")$approval_status`
#' @param last_modified `r get_ogc_params("latest-continuous")$last_modified`
#'
#' See also Details below for more information.
#' @param time_series_id `r get_ogc_params("latest-continuous")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param qualifier `r get_ogc_params("latest-continuous")$qualifier`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("latest-continuous", "latest_continuous_id")`.
#' The default (`NA`) will return all columns of the data.
#' @inheritParams check_arguments_api
#' @inheritParams check_arguments_non_api
#'
#' @inherit read_waterdata_continuous details
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#' site <- "USGS-451605097071701"
#' pcode <- "72019"
#' uv_data_sf <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                               parameter_code = pcode)
#'
#' uv_data_trim <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                           parameter_code = pcode,
#'                           properties = c("monitoring_location_id",
#'                                          "value",
#'                                          "time"))
#'
#' uv_data <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                            parameter_code = pcode,
#'                            skipGeometry = TRUE)
#'
#' uv_data_period <- read_waterdata_latest_continuous(monitoring_location_id = site,
#'                                   parameter_code = pcode,
#'                                   time = "P7D")
#'
#' multi_site <- read_waterdata_latest_continuous(monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-14181500"),
#'                               parameter_code = c("00060", "72019"),
#'                               skipGeometry = TRUE)
#'
#' # Only return data that has been modified in last 7 days
#' multi_site2 <- read_waterdata_latest_continuous(monitoring_location_id =  c("USGS-451605097071701",
#'                                                                            "USGS-14181500"),
#'                                                parameter_code = c("00060", "72019"),
#'                                                last_modified = "P7D")
#'
#' }
read_waterdata_latest_continuous <- function(
  monitoring_location_id = NA_character_,
  parameter_code = NA_character_,
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
  chunk_size = getOption("dataRetrieval.site_chunk_size_meta"),
  limit = getOption("dataRetrieval.limit"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "latest-continuous"
  output_id <- "latest_continuous_id"
  rlang::check_dots_empty()

  args <- mget(names(formals()))
  return_list <- get_ogc_data(args, output_id, service)

  return(return_list)
}
