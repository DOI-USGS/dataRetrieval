#' Get Continuous USGS Water Data
#' 
#' @description `r get_description("continuous")`
#' 
#' Currently, the services only allow up to 3 years of data to be requested with
#' a single request. If no "time" is specified, the service will return the 
#' last single year of data. If this is a bottleneck, please check back 
#' for new direct download functions that are expected to be available sometime
#' in 2026.
#' 
#' Geometry output is not supported in the continuous data API endpoint.
#' 
#' @export
#' @param monitoring_location_id `r get_ogc_params("continuous")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("continuous")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param time `r get_ogc_params("continuous")$time`
#' 
#' See also Details below for more information.
#' 
#' @param value `r get_ogc_params("continuous")$value`
#' @param unit_of_measure `r get_ogc_params("continuous")$unit_of_measure`
#' @param approval_status `r get_ogc_params("continuous")$approval_status`
#' @param last_modified `r get_ogc_params("continuous")$last_modified`
#' 
#' See also Details below for more information.
#' @param time_series_id `r get_ogc_params("continuous")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param qualifier `r get_ogc_params("continuous")$qualifier`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("continuous", "continuous_id")`.
#' The default (`NA`) will return all columns of the data.
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector, and sepcifically
#' order the returning data frame by time and monitoring_location_id.
#' @param no_paging logical, defaults to `FALSE`. If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution. 
#' 
#' @details 
#' You can also use a vector of length 2 for any time queries (such as time
#' or last_modified). The first value is the starting date (or datetime), 
#' the second value is the ending date(or datetime).
#' NA's within the vector indicate a half-bound date. 
#' For example, \code{time = c("2024-01-01", NA)} will return all data starting
#' at 2024-01-01. 
#' \code{time = c(NA, "2024-01-01")} will return all data from the beginning of 
#' the timeseries until 2024-01-01.
#' By default, time is assumed UTC, although time zone attributes
#' will be accommodated. As an example, setting \code{time = as.POSIXct(c("2021-01-01 12:00:00",
#' "2021-01-01 12:00"), tz = "America/New_York")} will request data that between
#' noon and 2pm eastern time on 2021-01-01.
#' All time values RETURNED from the service are UTC with the exception of 
#' daily data, which returns time values in local dates.
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-451605097071701"
#' pcode <- "72019"
#'
#' uv_data_trim <- read_waterdata_continuous(monitoring_location_id = site,
#'                            parameter_code = pcode, 
#'                            properties = c("value", "time"),
#'                            time = as.POSIXct(c("2026-02-07 12:00", 
#'                                                "2026-02-08 12:00"), 
#'                                               tz = "America/Chicago"))
#'
#' uv_data <- read_waterdata_continuous(monitoring_location_id = site,
#'                            parameter_code = pcode,
#'                            time = "P2D")
#' 
#' 
#' # Only return data that has been modified in last 7 days         
#' multi_site2 <- read_waterdata_continuous(monitoring_location_id =  c("USGS-451605097071701",
#'                                                                      "USGS-14181500"),
#'                                                parameter_code = c("00060", "72019"),
#'                                                last_modified = "P7D")
#' 
#' }
read_waterdata_continuous <- function(monitoring_location_id = NA_character_,
                                      parameter_code = NA_character_,
                                      properties = NA_character_,
                                      time_series_id = NA_character_,
                                      approval_status = NA_character_,
                                      unit_of_measure = NA_character_,
                                      qualifier = NA_character_,
                                      value = NA,
                                      last_modified = NA_character_,
                                      time = NA_character_,
                                      limit = NA,
                                      convertType = TRUE,
                                      no_paging = FALSE){
  
  service <- "continuous"
  output_id <- "continuous_id"
  
  args <- mget(names(formals()))
  args[["skipGeometry"]] <- TRUE

  return_list <- get_ogc_data(args,
                              output_id, 
                              service)

  attr(return_list$time, "tzone") <- "UTC"
  return(return_list)
}



