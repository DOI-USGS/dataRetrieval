#' Get period of record continuous data
#'
#' Using the STAC Water Data service, get full period of record
#' returns for continuous data. The most recent data may be missing,
#' but can be retrieved using `read_waterdata_continuous`.
#'
#' @param monitoring_location_id Each monitoring location in the USGS data base has a unique 8- to 15-digit identification number.
#' @param time_series_id `r get_ogc_params("daily")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param approval_status `r get_ogc_params("daily")$approval_status`
#' @param parameter_code `r get_ogc_params("daily")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
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
#' from the STAC search.
#' @param memory_MB_limit If the returned data is roughly bigger than this argument,
#' then the function will error out. Increasing this value will allow bigger queries
#' to be retrieved.
#'
#' @export
#' @seealso [fix_continuous_overlap()]
#' @examplesIf is_dataRetrieval_user()
#'
#' \donttest{
#'
#' df <- read_waterdata_continuous_por(
#'     monitoring_location_id = "USGS-0208458892"
#' )
#' nrow(df)
#'
#'
#' # Multiple sites:
#' df_2 <- read_waterdata_continuous_por(
#'     monitoring_location_id = c("USGS-01435000", "USGS-05406360")
#' )
#' nrow(df_2)
#'
#' # Multiple sites, limited parameter_codes:
#' df_3 <- read_waterdata_continuous_por(
#'     monitoring_location_id = c("USGS-01435000", "USGS-05406360"),
#'     parameter_code = c("00060", "00065")
#' )
#' nrow(df_3)
#' range(df_3$time)
#'
#' # Multiple sites, limited parameter_codes, limited time:
#' df_4 <- read_waterdata_continuous_por(
#'     monitoring_location_id = c("USGS-01435000", "USGS-05406360"),
#'     parameter_code = c("00060", "00065"),
#'     start_time = as.POSIXct("2015-01-01", tz = "UTC")
#' )
#' nrow(df_4)
#' range(df_4$time)
#'
#' # HUCs
#' df_huc <- read_waterdata_continuous_por(
#'     hydrologic_unit_code = "0709000214",
#'     parameter_code = "00060",
#'     start_time = as.POSIXct("2020-01-01", tz = "UTC")
#' )
#' nrow(df_huc)
#'
#' }
read_waterdata_continuous_por <- function(
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
  ...,
  site_limits = 1000,
  memory_MB_limit = 1000 #Stop if expected results exceed approximately 1 GB
) {
  rlang::check_dots_empty()

  ts_meta <- read_waterdata_combined_meta(
    monitoring_location_id = monitoring_location_id,
    county_name = county_name,
    state_name = state_name,
    site_type = site_type,
    bbox = bbox,
    hydrologic_unit_code = hydrologic_unit_code,
    site_type_code = site_type_code,
    id = time_series_id,
    parameter_code = parameter_code,
    data_type = "Continuous values"
  )

  if (length(unique(ts_meta$monitoring_location_id)) > site_limits) {
    stop("Unique monitoring_location_id exceed site_limits")
  }

  search_features <- list(
    monitoring_location_id = unique(ts_meta$monitoring_location_id),
    collections = "continuous",
    limit = site_limits,
    chunk_size = 50
  )

  search_features <- search_features[!is.na(search_features)]

  features <- suppressMessages(do.call(search_stac, search_features))

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
    statistic_id = NA_character_
  )

  check_memory(df_out, memory_MB_limit)

  df_out_loaded <- df_out |> dplyr::collect()

  # Continuous data will have overlap between historical and hot:
  df_out_loaded <- fix_continuous_overlap(df_out_loaded)

  return(df_out_loaded)
}

#' @importFrom rlang .data
filter_stac <- function(
  df_out,
  parameter_code,
  time_series_id,
  start_time,
  end_time,
  approval_status,
  statistic_id
) {
  if (!all(is.na(parameter_code))) {
    df_out <- df_out |>
      dplyr::filter(.data$parameter_code %in% !!parameter_code)
  }

  if (!all(is.na(time_series_id))) {
    df_out <- df_out |>
      dplyr::filter(.data$time_series_id %in% !!time_series_id)
  }

  if (!all(is.na(start_time))) {
    df_out <- df_out |>
      dplyr::filter(.data$time >= !!start_time)
  }

  if (!all(is.na(end_time))) {
    df_out <- df_out |>
      dplyr::filter(.data$time <= !!end_time)
  }

  if (!all(is.na(approval_status))) {
    df_out <- df_out |>
      dplyr::filter(.data$approval_status %in% !!approval_status)
  }

  if (!all(is.na(statistic_id))) {
    df_out <- df_out |>
      dplyr::filter(.data$statistic_id %in% !!statistic_id)
  }

  return(df_out)
}

#' Fix overlap between historic and current continuous data
#'
#' Fix overlap between historic and current continuous data. This is
#' needed because sometimes the historic and hot data will overlap.
#' This will make sure the data with the more recent "last_modified"
#' will have priority.
#'
#'
#' @export
#' @param df Data frame with at least time_series_id, time, last_modified
#' @return Data frame with duplicated data removed
#' @examples
#'
#' df1 <- data.frame(
#'      time = c(seq.Date("2026-01-01", "2026-01-07"),
#'               seq.Date("2026-01-05", "2026-01-09")),
#'      last_modified = c(rep("2026-01-07", 7),
#'                        rep("2026-01-09", 5)),
#'      value = 1:12,
#'      time_series_id = "a"
#' )
#'
#' fix_continuous_overlap(df1)
#'
fix_continuous_overlap <- function(df) {
  no_dups <- data.table::uniqueN(data.table::data.table(df[, c(
    "time_series_id",
    "time"
  )])) ==
    nrow(df)
  if (!no_dups) {
    # For each time_series_id/time pair, keep only the row with the
    # most recent last_modified value, dropping the older duplicates.
    was_data_frame <- !data.table::is.data.table(df)

    dt <- data.table::as.data.table(df)

    # Stable row index so original order can be restored after de-duping.
    dt[, ".orig_order" := .I]

    # Order so that, within each group, the newest last_modified is last,
    # then keep the last row of each time_series_id/time group.
    data.table::setorderv(dt, c("time_series_id", "time", "last_modified"))
    dt <- dt[,
      .SD[.N],
      by = c("time_series_id", "time")
    ]

    # Restore original row ordering and remove helper column.
    data.table::setorderv(dt, ".orig_order")
    dt[, ".orig_order" := NULL]

    if (was_data_frame) {
      df_out <- as.data.frame(dt)
    } else {
      df_out <- dt
    }
    return(df_out)
  } else {
    return(df)
  }
}


check_memory <- function(df_out, memory_MB_limit) {
  n_rows_expected <- dplyr::count(df_out) |> dplyr::collect()
  memory_size_bytes <- as.numeric(n_rows_expected$n) * ncol(df_out) * 8
  memory_size_megabytes <- memory_size_bytes / (1024^2) # Convert bytes to megabytes

  if (memory_size_megabytes > memory_MB_limit) {
    message(
      "Estimated memory size of the dataset:",
      memory_size_megabytes,
      "MB\n"
    )
    stop(
      "Increase the memory_MB_limit parameter to allow this query to be loaded."
    )
  }
}
