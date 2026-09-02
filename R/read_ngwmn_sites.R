#' Get NGWMN Site Data
#'
#' @description `r get_description("sites", base = "NGWMN")`
#'
#' @export
#' @param monitoring_location_id
#' `r get_ogc_params("sites", base = "NGWMN")$monitoring_location_id$description`
#' @param agency_code
#' `r get_ogc_params("sites", base = "NGWMN")$agency_code$description`
#' @param monitoring_location_number
#' `r get_ogc_params("sites", base = "NGWMN")$monitoring_location_number$description`
#' @param altitude
#' `r get_ogc_params("sites", base = "NGWMN")$altitude$description`
#' @param national_aquifer_code Code for U.S. Principal Aquifer. A list of values
#' can be passed for this field, separated by commas.
#' @param national_aquifer_description
#' `r get_ogc_params("sites", base = "NGWMN")$national_aquifer_description$description`
#' @param country_code
#' `r get_ogc_params("sites", base = "NGWMN")$country_code$description`
#' @param country_name
#' `r get_ogc_params("sites", base = "NGWMN")$country_name$description`
#' @param state_name
#' `r get_ogc_params("sites", base = "NGWMN")$state_name$description`
#' @param county_name
#' `r get_ogc_params("sites", base = "NGWMN")$county_name$description`
#' @param aquifer_name
#' `r get_ogc_params("sites", base = "NGWMN")$aquifer_name$description`
#' @param site_type
#' `r get_ogc_params("sites", base = "NGWMN")$site_type$description`
#' @param aquifer_type_code
#' `r get_ogc_params("sites", base = "NGWMN")$aquifer_type_code$description`
#' @param qw_sys_name
#' `r get_ogc_params("sites", base = "NGWMN")$qw_sys_name$description`
#' @param qw_sn_flag
#' `r get_ogc_params("sites", base = "NGWMN")$qw_sn_flag$description`
#' @param qw_baseline_flag
#' `r get_ogc_params("sites", base = "NGWMN")$qw_baseline_flag$description`
#' @param qw_well_chars
#' `r get_ogc_params("sites", base = "NGWMN")$qw_well_chars$description`
#' @param qw_well_type
#' `r get_ogc_params("sites", base = "NGWMN")$qw_well_type$description`
#' @param qw_well_purpose
#' `r get_ogc_params("sites", base = "NGWMN")$qw_well_purpose$description`
#' @param wl_sys_name
#' `r get_ogc_params("sites", base = "NGWMN")$wl_sys_name$description`
#' @param wl_sn_flag
#' `r get_ogc_params("sites", base = "NGWMN")$wl_sn_flag$description`
#' @param wl_baseline_flag
#' `r get_ogc_params("sites", base = "NGWMN")$wl_baseline_flag$description`
#' @param wl_well_chars
#' `r get_ogc_params("sites", base = "NGWMN")$wl_well_chars$description`
#' @param wl_well_type
#' `r get_ogc_params("sites", base = "NGWMN")$wl_well_type$description`
#' @param wl_well_purpose
#' `r get_ogc_params("sites", base = "NGWMN")$wl_well_purpose$description`
#'
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are:
#' `r dataRetrieval:::get_properties_for_docs("sites", base = "NGWMN")`.
#' The default (`NA`) will return all columns of the data.
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.

#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
#' vector structured: c(xmin,ymin,xmax,ymax). Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param \dots Not used. Included to help differentiate official NGWMN API arguments
#' from more seldom used, optional dataRetrieval-specific arguments.
#' @inheritParams check_arguments_non_api
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' ngwmn_sites <- read_ngwmn_sites(state_name = "Minnesota")
#'
#' org_type <- read_ngwmn_sites(agency_code = "MN_DNR",
#'                              county_name = "Washington County")
#'
#' }
read_ngwmn_sites <- function(
  monitoring_location_id = NA_character_,
  agency_code = NA_character_,
  monitoring_location_number = NA_character_,
  altitude = NA_character_,
  national_aquifer_code = NA_character_,
  national_aquifer_description = NA_character_,
  country_code = NA_character_,
  country_name = NA_character_,
  state_name = NA_character_,
  county_name = NA_character_,
  aquifer_name = NA_character_,
  site_type = NA_character_,
  aquifer_type_code = NA_character_,
  qw_sys_name = NA_character_,
  qw_sn_flag = NA_character_,
  qw_baseline_flag = NA_character_,
  qw_well_chars = NA_character_,
  qw_well_type = NA_character_,
  qw_well_purpose = NA_character_,
  wl_sys_name = NA_character_,
  wl_sn_flag = NA_character_,
  wl_baseline_flag = NA_character_,
  wl_well_chars = NA_character_,
  wl_well_type = NA_character_,
  wl_well_purpose = NA_character_,
  bbox = NA,
  properties = NA_character_,
  skipGeometry = FALSE,
  ...,
  convertType = getOption("dataRetrieval.convertType"),
  no_paging = getOption("dataRetrieval.no_paging"),
  chunk_size = getOption("dataRetrieval.site_chunk_size_data"),
  limit = getOption("dataRetrieval.limit"),
  attach_request = getOption("dataRetrieval.attach_request")
) {
  service <- "sites"
  rlang::check_dots_empty()
  args <- mget(names(formals()))

  return_list <- get_ogc_data(
    args = args,
    output_id = "id",
    service = service,
    base = "NGWMN"
  )

  return(return_list)
}
