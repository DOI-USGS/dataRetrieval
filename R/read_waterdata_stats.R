#' Get USGS daily data statistics
#'
#' @description
#'
#' This service provides endpoints for access to computations on the historical
#' record regarding water conditions. For more information regarding the
#' calculation of statistics and other details, please visit the Statistics
#' documentation page.
#' 
#' Note: The /statistics API is under active beta development and subject to change.
#' Improved handling of significant figures will be addressed in a future
#' release.
#'
#' \code{read_waterdata_stats_por} Returns day-of-year and month-of-year
#' statistics matching your query.
#'
#' \code{read_waterdata_stats_daterange} Returns monthly and annual statistics matching
#' your query.
#'
#' @export
#'
#' @param approval_status Whether to include approved and/or provisional
#'   observations. At this time, only approved observations are returned.
#' @param computation_type Desired statistical computation method. Available
#'   values: "arithmetic_mean", "maximum", "median", "minimum", "percentile".
#' @param country_code Country Query Parameter. Accepts multiple values (see
#'   examples). If one of country, county, or state code is supplied then the
#'   other two arguments do not need to be specified.
#' @param state_code State Query Parameter. Accepts multiple values in a
#'   character vector.
#' @param county_code County Query Parameter. Accepts multiple values in a
#'   character vector.
#' @param start_date Start Date Query Parameter. The logic is inclusive i.e., it
#'   will also return records that match the date. If an end date is supplied,
#'   but no start date is supplied, then statistics will be supplied for the
#'   entire period of record ending with the end date. If an end date is not
#'   supplied, but a start date is supplied then statistics will be supplied for
#'   the period of record following the start date. If no start or end date are
#'   supplied then statistics will be supplied for the entire period of record.
#' @param end_date End Date Query Parameter. The logic is inclusive i.e., it will
#'   also return records that match the date.
#' @param monitoring_location_id Each monitoring location has been assigned a
#'   unique station number that places them in downstream order. Accepts
#'   multiple values in a character vector.
#' @param parent_time_series_id The parent_time_series_id returns statistics
#'   tied to a particular database entry. Accepts multiple values in a character
#'   vector. If no parent time series identifier is supplied, then all records
#'   matching the rest of the provided criteria will be returned.
#' @param site_type_code Site Type Code Query Parameter. Accepts multiple values
#'   in a character vector. If no Site Type code is specified, statistics of all
#'   site types with the matching Monitoring Location Identifier will be
#'   returned.
#' @param site_type_name Site Type Name Query Parameter. If no Site Type name is
#'   specified, statistics of all site types with the matching Monitoring
#'   Location Identifier will be returned.
#' @param parameter_code USGS Parameter Code Query Parameter. Accepts multiple
#'   values in a character vector. If no USGS parameter code is specified, but a
#'   Monitoring Location Identifier is supplied, then all statistics and their
#'   parameter codes with a matching monitoring location identifier will be
#'   returned. All statistics within the period of record will be returned if no
#'   parameter code or monitoring location identifier are specified.
#' @param page_size Return a defined number of results (default: 1000).
#'
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' # All day-of-year and month-of-year statistics for two sites
#' x1 <- read_waterdata_stats_por(
#'   monitoring_location_id = c("USGS-02319394", "USGS-02171500")
#' )
#' 
#' # Request temperature percentiles for specific month-day range
#' # Returns:
#' # - Day-of-year temperature percentiles for each day between June 1 through June 15.
#' # - Month-of-year percentiles for June, computed using 
#' #    all June data (not just June 1 through June 15).
#' # Note: the month-of-year percentiles are returned only when the month-day range includes 
#' #  the beginning of the month (e.g., "06-01") 
#' x2 <- read_waterdata_stats_por(
#'   monitoring_location_id = c("USGS-02319394", "USGS-02171500"),
#'   parameter_code = "00010", 
#'   start_date = "06-01", end_date = "06-15",
#'   computation_type = "percentile"
#' )
#' 
#' # All calendar month, calendar year, and water year statistics for two sites
#' x3 <- read_waterdata_stats_daterange(
#'   monitoring_location_id = c("USGS-02319394", "USGS-02171500")
#' )
#' 
#' # Request specific gage height and discharge summaries for a limited date range
#' # Returns:
#' # - calendar month summaries for each month between January, 2010 through December, 2011
#' # - calendar year summaries for 2010 and 2011
#' # - water year summaries for WY2010, WY2011, and WY2012
#' x4 <- read_waterdata_stats_daterange(
#'   monitoring_location_id = c("USGS-02319394", "USGS-02171500"), 
#'   parameter_code = c("00065", "00060"),
#'   start_date = "2010-01-01", end_date = "2011-12-31",
#'   computation_type = c("minimum", "median", "maximum")
#' )
#' 
#' }
#' 
#' @rdname read_waterdata_stats
#' @seealso \url{https://api.waterdata.usgs.gov/statistics/v0/docs}
read_waterdata_stats_por <- function(
    approval_status = NA,
    computation_type = NA_character_,
    country_code = NA_character_,
    state_code = NA_character_,
    county_code = NA_character_,
    start_date = NA_character_,
    end_date = NA_character_,
    monitoring_location_id = NA_character_,
    parent_time_series_id = NA_character_,
    site_type_code = NA_character_,
    site_type_name = NA_character_,
    parameter_code = NA_character_,
    page_size = NA
) {
  
  args <- mget(names(formals()))
  
  get_statistics_data(args = args, service = "Normals")
}

#' @export
#' @rdname read_waterdata_stats
read_waterdata_stats_daterange <- function(
    approval_status = NA,
    computation_type = NA_character_,
    country_code = NA_character_,
    state_code = NA_character_,
    county_code = NA_character_,
    start_date = NA_character_,
    end_date = NA_character_,
    monitoring_location_id = NA_character_,
    parent_time_series_id = NA_character_,
    site_type_code = NA_character_,
    site_type_name = NA_character_,
    parameter_code = NA_character_,
    page_size = NA
){
  
  args <- mget(names(formals()))
  
  get_statistics_data(args = args, service = "Intervals")
  
}

#' Create a request object for the /statistics service
#' 
#' @param service chr; "Normals" or "Intervals"
#' @param version int; /statistics API version number (default: 0)
#' 
#' @return a httr2 request object
#' 
#' @noRd
construct_statistics_request <- function(service = "Normals", version = 0){
  
  httr2::request("https://api.waterdata.usgs.gov/statistics/") |>
    httr2::req_user_agent(default_ua()) |>
    httr2::req_headers(`Accept-Encoding` = "gzip") |>
    httr2::req_timeout(seconds = 180) |>
    httr2::req_url_path_append(paste0("v", version)) |>
    httr2::req_url_path_append(paste0("observation", service))
  
}

#' Retrieve data from /statistics API
#' 
#' @param args arguments from individual functions.
#' @param service Endpoint name.
#' 
#' @noRd
#' @return data.frame with attributes
get_statistics_data <- function(args, service) {

  rid <- data <- ts_id <- values <- NULL

  base_request <- construct_statistics_request(service = service, version = 0)
  
  full_request <- explode_query(base_request, POST = FALSE, x = args)
  
  message("Requesting:\n", full_request$url)

  return_list <- data.table::as.data.table(walk_pages(full_request))
  
  if(nrow(return_list) == 0) {
    return(deal_with_empty_stats(return_list))
  }
  
  return_list[, rid := .I]
  
  parsed_data <- lapply(return_list$data, jsonlite::parse_json)
  return_list[, data := NULL]
  
  combined_list <- list()
  for (i in seq_along(parsed_data)){
    # 1. One row per time series
    ts_meta <- data.table::rbindlist(parsed_data[[i]], fill = TRUE)
    ts_meta[, ts_id := .I]  # local key
    
    # 2. One row per observation, keyed to ts_id
    obs <- data.table::rbindlist(ts_meta$values, fill = TRUE, idcol = "ts_id")

    obs <- cleanup_cols_stats(obs)
    
    # 3. Drop nested list column
    ts_meta[, values := NULL]
    
    # 4. Join
    out <- ts_meta[obs, on = "ts_id", allow.cartesian = TRUE]
    
    out[, `:=`(
      ts_id = NULL,
      rid   = i
    )]
    
    combined_list[[i]] <- out
  }
  
  combined <- data.table::rbindlist(combined_list, use.names = TRUE)
  combined <- combined[return_list, on = "rid"]
  combined[, rid := NULL]
  
  combined <- sf::st_as_sf(as.data.frame(combined))
  
  attr(combined, "request") <- full_request
  attr(combined, "queryTime") <- Sys.time()
  
  return(combined)
}

#' Clean up "value", "values", and "percentiles" columns in a data.table
#'
#' @description If the input data.table has "values" and "percentiles" columns,
#'   then it unnests both, making the data.table longer (one row per value)
#'
#'   If the input data.table *also* has a "value" column, then it consolidates
#'   with "values", yielding a single "value" column
#'
#'   The "percentiles" column is replaced by "percentile" (matching
#'   singularization of "value"). The function also checks whether the
#'   "computation" column contains "minimum", "median", or "maximum" and sets
#'   the corresponding "percentile" to 0, 50, or 100, respectively. Note that
#'   the percentile column might only contain NAs if the input data.table only
#'   includes arithmetic_mean values.
#'
#'   Lastly, the value and percentile columns are converted from character to
#'   numeric
#'
#' @note This function is intended to evaluate as a data.table j expression,
#'   which is why it doesn't accept any arguments. j expressions are typically
#'   written inline with the data.table definition (e.g., \code{DT[, { do
#'   something }]}). However, this expression would have been excessively long.
#'   Instead, a substitute() is used so this could be pulled out into a separate
#'   function for readability, but we still gain the computational benefits of
#'   using an expression.
#'
#' @noRd
#' @return data.table object with "value" and "percentile" columns
#'
#' @seealso
#' \url{https://stat.ethz.ch/CRAN/web/packages/data.table/vignettes/datatable-programming.html}
cleanup_cols_stats <- function(df){
 
  percentiles <- percentile <- values <- computation <- NULL
  ## ---- detect column presence ----
  has_value       <- "value" %in% names(df)
  has_values      <- "values" %in% names(df)
  has_percentiles <- "percentiles" %in% names(df)

  if(has_value){
    df[, value := as.numeric(value)]
  } else {
    df[, value := NA_real_]
  }
  
  if(has_percentiles){
    df[, percent := NA_integer_]
    df[is.na(value), percent := as.integer(percentiles)][, percentiles := NULL]
    data.table::setnames(df, "percent", "percentile")
  } else {
    df[, percentile := NA_integer_]
  }
  
  # value has already been created, so this will only move values if needed
  if(has_values){
    df[is.na(value), value := as.numeric(values)][, values := NULL]
  }

  df$value[is.nan(df$value)] <- NA_real_

  ## ---- percentiles ----
  if (any(data.table::`%chin%`(df$computation, c("minimum", "median", "maximum")))){
    df[, percentile := data.table::fcase(
      computation == "minimum", 0L,
      computation == "median", 50L,
      computation == "maximum", 100L,
      default = percentile
    )]
  } 
  
  return(df)
}

#' Handle empty responses from the /statistics service
#'
#' @param return_list data.frame returned from walk_pages
#' @param properties character vector of requested columns (or NA)
#' @param convertType logical, whether to convert numeric columns
#'
#' @return data.frame or sf object with expected columns
#' @noRd
deal_with_empty_stats <- function(return_list, properties = NA,
                                  convertType = TRUE) {
  
  # Define default columns for stats service
  default_columns <- c(
    "monitoring_location_id", "monitoring_location_name",
    "site_type", "site_type_code", "country_code", "state_code",
    "county_code", "parameter_code", "unit_of_measure", "parent_time_series_id",
    "value", "percentile", "start_date", "end_date", "interval_type",
    "sample_count", "approval_status", "computation_id", "computation"
  )
  
  if (all(is.na(properties))) {
    properties <- default_columns
  }
  
  # create empty data.frame
  return_list <- as.data.frame(matrix(nrow = 0, ncol = length(properties)))
  names(return_list) <- properties
  return_list <- lapply(return_list, as.character)
  return_list <- as.data.frame(return_list)
  
  # convert numeric columns if requested
  if (convertType) {
    numeric_cols <- c("value", "percentile", "sample_count")
    for (col in numeric_cols) {
      if (col %in% names(return_list)) {
        return_list[[col]] <- as.numeric(return_list[[col]])
      }
    }
  }
  
  # ensure geometry column exists if not skipped
  return_list <- sf::st_as_sf(return_list, geometry = sf::st_sfc())
  
  return(return_list)
}

