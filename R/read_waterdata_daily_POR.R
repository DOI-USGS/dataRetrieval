#' Get period of record daily data
#'
#' Using the STAC Water Data service, get full period of record
#' returns for daily data. The most recent data may be missing,
#' but can be retrieved using `read_waterdata_daily`.
#'
#' @param monitoring_location_id Each monitoring location in the USGS data base has a unique 8- to 15-digit identification number.
#' @param time_series_id `r get_ogc_params("daily")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param approval_status `r get_ogc_params("daily")$approval_status`
#' @param parameter_code `r get_ogc_params("daily")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param statistic_id `r get_ogc_params("daily")$statistic_id`
#' Multiple statistic_ids can be requested as a character vector.
#' @param county_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$county_name$description`
#' @param state_name `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$state_name$description`
#' @param site_type `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$site_type$description`
#' @param site_type_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$site_type_code$description`
#' @param hydrologic_unit_code `r check_OGC_requests(endpoint = "daily", type = "queryables")$properties$hydrologic_unit_code$description`
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric
#' vector structured: c(xmin,ymin,xmax,ymax).
#' Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param start_time Earliest time returned
#' @param end_time Latest time returned
#' @param \dots Not used.
#' @param site_limits Set a limit to how many sites will be returned
#' @param memory_MB_limit If the returned data is roughly bigger than this argument,
#' then the function will error out. Increasing this value will allow bigger queries
#' to be retrieved.
#' from the STAC search.
#'
#' @export
#' @inherit read_waterdata_continuous details
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' df <- read_waterdata_daily_por(
#'     monitoring_location_id = "USGS-0208458892"
#' )
#' nrow(df)
#'
#' # Multiple sites:
#' df_2 <- read_waterdata_daily_por(
#'     monitoring_location_id = c("USGS-01435000", "USGS-05406360")
#' )
#' nrow(df_2)
#'
#' # Multiple sites, limited parameter_codes:
#' df_3 <- read_waterdata_daily_por(
#'     monitoring_location_id = c("USGS-01435000", "USGS-05406360"),
#'     parameter_code = c("00060", "00065")
#' )
#' nrow(df_3)
#' range(df_3$time)
#'
#' # Multiple sites, limited parameter_codes, limited time:
#' df_4 <- read_waterdata_daily_por(
#'     monitoring_location_id = c("USGS-01435000", "USGS-05406360"),
#'     parameter_code = c("00060", "00065"),
#'     start_time = as.POSIXct("1960-01-01", tz = "UTC"),
#'     approval_status = "Approved"
#' )
#' nrow(df_4)
#' range(df_4$time)
#' }
read_waterdata_daily_por <- function(
  monitoring_location_id = NA_character_,
  county_name = NA_character_,
  state_name = NA_character_,
  site_type = NA_character_,
  site_type_code = NA_character_,
  hydrologic_unit_code = NA_character_,
  bbox = NA,
  time_series_id = NA_character_,
  parameter_code = NA_character_,
  start_time = NA_character_,
  end_time = NA_character_,
  approval_status = NA_character_,
  statistic_id = NA_character_,
  ...,
  site_limits = 1000,
  memory_MB_limit = 1000
) {
  rlang::check_dots_empty()

  search_features <- list(
    monitoring_location_id = monitoring_location_id,
    county_name = county_name,
    state_name = state_name,
    site_type = site_type,
    bbox = bbox,
    hydrologic_unit_code = hydrologic_unit_code,
    site_type_code = site_type_code,
    collections = "daily",
    limit = site_limits
  )

  search_features <- search_features[!is.na(search_features)]

  features <- do.call(search_stac, search_features)

  con_1 <- initial_connection()
  on.exit(DBI::dbDisconnect(con_1))

  df_out <- open_stac(
    features = features,
    con = con_1
  )

  df_out <- filter_stac(
    df_out = df_out,
    parameter_code = parameter_code,
    time_series_id = time_series_id,
    start_time = start_time,
    end_time = end_time,
    approval_status = approval_status,
    statistic_id = statistic_id
  )

  check_memory(df_out, memory_MB_limit)

  df_out_loaded <- df_out |> dplyr::collect()
  return(df_out_loaded)
}
