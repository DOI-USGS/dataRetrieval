#' 
get_statistics_data_data_table <- function(args, service) {
  
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
    observations <- observations[, eval(clean_value_cols()), by = .I][, I := NULL]
    time_series_metainfo[, values := NULL]
    
    time_series_metainfo <- cbind(time_series_metainfo, observations)
    time_series_metainfo[, rid := i]

    combined_list[[i]] <- time_series_metainfo
  }
  
  combined <- data.table::rbindlist(combined_list)
  combined <- combined[return_list, on = "rid"]
  
  col_order <- c("parent_time_series_id", "monitoring_location_id", "monitoring_location_name", "parameter_code",
               "interval_type", "start_date", "end_date", "computation", "value", "percentile",
               "unit_of_measure", "sample_count",
               "approval_status", "computation_id", 
               "site_type", "site_type_code", 
               "country_code", "state_code", "county_code", 
               "geometry")
  data.table::setcolorder(combined, col_order) 
   
  attr(combined, "request") <- full_request
  attr(combined, "queryTime") <- Sys.time()
  
  return(sf::st_as_sf(combined))
}

#' Clean up "value", "values", and "percentiles" columns in a data.table
#' 
#' @description
#' If the input data.table has "values" and "percentiles" columns, then 
#' it unnests both, making the data.table longer (one row per value)
#' 
#' If the input data.table *also* has a "value" column, then it consolidates with "values", yielding a single "value" column
#' 
#' The "percentiles" column is replaced by "percentile" (matching singularization of "value").
#' The function also checks whether the "computation" column contains "minimum", "median", or "maximum" and 
#' sets the corresponding "percentile" to 0, 50, or 100, respectively. Note that the percentile column might only 
#' contain NAs if the input data.table only includes arithmetic_mean values
#' 
#' Lastly, the value and percentile columns are converted from character to numeric
#'
#' 
#' @note
#' This function is intended to evaluate as a data.table j expression, which is why it doesn't accept any arguments.
#' j expressions are typically written inline with the data.table definition (e.g., DT[, { do something }]). 
#' However, this expression would have been excessively long. Instead, an eval(quote(...)) is used so this could be 
#' pulled out into a separate function for readability, but we still gain the computational benefits of using an expression.
#' 
#' @noRd
#' @return data.table object with "value" and "percentile" columns
clean_value_cols <- function() {
  quote({

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

    } else if (data.table::`%chin%`(computation,c("minimum", "medium", "maximum"))) {

      fifelse(
        computation == "minimum", 0,
        fifelse(computation == "medium", 50, 100)
      )

    } else {
      NA_real_
    }

    if (length(percs) == 1L && n > 1L) {
      percs <- rep(percs, n)
    }

    ## ---- expand scalar columns ----
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