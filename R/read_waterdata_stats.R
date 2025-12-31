#' Get summary statistic data from /statistics API
#'
#' @description
#'
#' This service provides endpoints for access to computations on the historical
#' record regarding water conditions. For more information regarding the
#' calculation of statistics and other details, please visit the Statistics
#' documentation page.
#'
#' Note: This API is under active beta development and subject to change.
#' Improved handling of significant figures will be addressed in a future
#' release.
#'
#' \code{read_waterdata_stats_normal} Returns day-of-year and month-of-year
#' statistics matching your query.
#'
#' \code{read_waterdata_stats} Returns monthly and annual statistics matching
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
#' @rdname get_waterdata_stats
#' @seealso \url{https://api.waterdata.usgs.gov/statistics/v0/docs}
read_waterdata_stats_normal <- function(
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
#' @rdname get_waterdata_stats
read_waterdata_stats_interval <- function(
    approval_status = NA,
    computation_type = NA_character_,
    country_code = NA_character_,
    state_code = NA_character_,
    county_code = NA_character_,
    start_date = NA_character_,
    end_date = NA_character_,
    mime_type = NA_character_,
    monitoring_location_id = NA_character_,
    next_token = NA_character_,
    parent_time_series_id = NA_character_,
    site_type_code = NA_character_,
    site_type_name = NA_character_,
    parameter_code = NA_character_,
    page_size = NA
){
  
  args <- mget(names(formals()))
  
  get_statistics_data(args = args, service = "Intervals")
  
}

#' Retrieve data from /statistics API
#' 
#' @param args arguments from individual functions.
#' @param service Endpoint name.
#' 
#' @noRd
#' @return data.frame with attributes
get_statistics_data <- function(args, service) {
  
  base_request <- construct_statistics_request(service = service, version = 0)
  
  # TODO?: arg type checking here
  
  full_request <- explode_query(base_request, POST = FALSE, x = args)
  
  return_list <- data.table::as.data.table(walk_pages(full_request, max_results = NA))
  return_list[, rid := .I]
  
  parsed_data <- lapply(return_list$data, jsonlite::parse_json)
  return_list[, data := NULL]
  
  combined_list <- list()
  for (i in seq_along(parsed_data)){
    time_series_metainfo <- data.table::rbindlist(parsed_data[[i]])
    
    observations <- data.table::rbindlist(time_series_metainfo$values, fill = TRUE)
    observations <- observations[, .j, by = .I, env = list(.j = clean_value_cols())][, I := NULL]
    time_series_metainfo[, values := NULL]
    
    time_series_metainfo <- cbind(time_series_metainfo, observations)
    time_series_metainfo[, rid := i]
    
    combined_list[[i]] <- time_series_metainfo
  }
  
  combined <- data.table::rbindlist(combined_list)
  combined <- combined[return_list, on = "rid"][, rid := NULL]
  
  attr(combined, "request") <- full_request
  attr(combined, "queryTime") <- Sys.time()
  
  return(sf::st_as_sf(as.data.frame(combined)))
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
clean_value_cols <- function() {
  substitute({
    
    ## ---- detect column presence ----
    has_value       <- exists("value")
    has_values      <- exists("values")
    has_percentiles <- exists("percentiles")
    
    ## ---- values ----
    vals <- if (has_values &&
                !is.null(values[[1]]) &&
                length(values[[1]]) > 0) {
      
      v <- values[[1]]
      if (length(v) == 1L && identical(v, "nan")) {
        NA_real_
      } else {
        as.numeric(v)
      }
      
    } else if (has_value) {
      as.numeric(value)
    } else {
      NA_real_
    }
    
    n <- length(vals)
    
    ## ---- percentiles ----
    percs <- if (has_percentiles &&
                 !is.null(percentiles[[1]]) &&
                 length(percentiles[[1]]) > 0) {
      
      as.numeric(percentiles[[1]])
      
    } else if (data.table::`%chin%`(computation,c("minimum", "median", "maximum"))) {
      
      data.table::fifelse(
        computation == "minimum", 0,
        data.table::fifelse(computation == "median", 50, 100)
      )
      
    } else {
      NA_real_
    }
    
    if (length(percs) == 1L && n > 1L) {
      percs <- rep(percs, n)
    }
    
    ## ---- expand  scalar columns ----
    .(
      value           = vals,
      percentile      = percs,
      start_date      = rep(start_date, n),
      end_date        = rep(end_date, n),
      interval_type   = rep(interval_type, n),
      sample_count    = rep(sample_count, n),
      approval_status = rep(approval_status, n),
      computation_id  = rep(computation_id, n),
      computation     = rep(computation, n)
    )
  })
}
