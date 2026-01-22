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
#' You can also use a vector of length 2: the first value being the starting date,
#' the second value being the ending date. NA's within the vector indicate a
#' half-bound date. For example, c("2024-01-01", NA) will return all data starting
#' at 2024-01-01.
#' @param value `r get_ogc_params("latest-daily")$value`
#' @param unit_of_measure `r get_ogc_params("latest-daily")$unit_of_measure`
#' @param approval_status `r get_ogc_params("latest-daily")$approval_status`
#' @param last_modified `r get_ogc_params("latest-daily")$last_modified`
#' @param time_series_id `r get_ogc_params("latest-daily")$time_series_id`
#' Multiple time_series_ids can be requested as a character vector.
#' @param qualifier `r get_ogc_params("latest-daily")$qualifier`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("latest-daily", "latest_daily_id")`.
#' The default (`NA`) will return all columns of the data.
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric 
#' vector structured: c(xmin,ymin,xmax,ymax). Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @param no_paging logical, defaults to `FALSE`. If `TRUE`, the data will
#' be requested from a native csv format. This can be dangerous because the
#' data will cut off at 50,000 rows without indication that more data
#' is available. Use `TRUE` with caution. 
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
#'                                                           "USGS-01645000"),
#'                               parameter_code = c("00060", "00010"))
#' 
#' }
read_waterdata_latest_daily <- function(monitoring_location_id = NA_character_,
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
                                 limit = NA,
                                 convertType = TRUE,
                                 no_paging = FALSE){
  
  service <- "latest-daily"
  output_id <- "latest_daily_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  return(return_list)
}



