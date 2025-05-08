#' Get USGS Daily Data
#' 
#' Description `r get_description("daily")`
#' 
#' @export
#' @param monitoring_location_id `r get_params("daily")$monitoring_location_id`
#' @param parameter_code `r get_params("daily")$parameter_code`
#' @param datetime `r get_params("daily")$datetime`
#' @param statistic_id `r get_params("daily")$statistic_id`
#' @param time `r get_params("daily")$time`
#' @param value `r get_params("daily")$value`
#' @param unit_of_measure `r get_params("daily")$unit_of_measure`
#' @param approval_status `r get_params("daily")$approval_status`
#' @param last_modified `r get_params("daily")$last_modified`
#' @param time_series_id `r get_params("daily")$time_series_id`
#' @param qualifier `r get_params("daily")$qualifier`
#' @param id `r get_params("daily")$id`
#' @param properties The properties that should be included for each feature.
#' The parameter value is a comma-separated list of property names. Available options are
#' `r schema <- check_OGC_requests(endpoint = "daily", type = "schema"); paste(names(schema$properties), collapse = ", ")`
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326.
#' @param crs Indicates the coordinate reference system for the results.
#' @param limit The optional limit parameter limits the number of items that are
#' presented in the response document. Only items are counted that are on the
#' first level of the collection in the response document. Nested objects
#' contained within the explicitly requested items shall not be counted.
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' pcode <- "00060"
#' dv_data_sf <- read_USGS_dv(monitoring_location_id = site,
#'                         parameter_code = "00060", 
#'                         datetime = c("2021-01-01", "2022-01-01"))
#'
#' dv_data_trim <- read_USGS_dv(monitoring_location_id = site,
#'                           parameter_code = "00060", 
#'                           properties = c("monitoring_location_id",
#'                                          "value",
#'                                          "time"),
#'                           datetime = c("2021-01-01", "2022-01-01"))
#'
#' dv_data <- read_USGS_dv(monitoring_location_id = site,
#'                         parameter_code = "00060",
#'                         skipGeometry = TRUE)
#' 
#' multi_site <- read_USGS_dv(monitoring_location_id =  c("USGS-01491000", 
#'                                                        "USGS-01645000"),
#'                         parameter_code = c("00060", "00010"),
#'                         datetime = c("2023-01-01", "2024-01-01"))
#' 
#' }
read_USGS_dv <- function(monitoring_location_id = NA_character_,
                         parameter_code = NA_character_,
                         statistic_id = NA_character_,
                         properties = c("monitoring_location_id",
                                        "parameter_code",
                                        "statistic_id",
                                        "time",
                                        "value",
                                        "unit_of_measure",
                                        "approval_status",
                                        "qualifier"),
                         bbox = NA,
                         time_series_id = NA_character_,
                         id = NA_character_,
                         approval_status = NA_character_,
                         unit_of_measure = NA_character_,
                         qualifier = NA_character_,
                         value = NA,
                         last_modified = NA_character_,
                         limit = 10000,
                         crs = NA_character_,
                         skipGeometry = NA,
                         datetime = NA_character_,
                         convertType = TRUE){
  
  message("Function in development, use at your own risk.")
  
  dv_req <- construct_api_requests(service = "daily",
                                   monitoring_location_id = monitoring_location_id,
                                   parameter_code = parameter_code,
                                   statistic_id = statistic_id,
                                   properties = properties,
                                   bbox = bbox,
                                   time_series_id = time_series_id,
                                   id = id,
                                   approval_status = approval_status,
                                   unit_of_measure = unit_of_measure,
                                   qualifier = qualifier,
                                   value = value,
                                   last_modified = last_modified,
                                   limit = limit,
                                   crs = crs,
                                   skipGeometry = skipGeometry,
                                   datetime = datetime)
  
  return_list <- walk_pages(dv_req)
  
  if(convertType) return_list <- cleanup_cols(return_list)
  
  return_list <- return_list[order(return_list$time, return_list$monitoring_location_id), ]
  
  if(!"id" %in% properties){
    return_list <- return_list[, names(return_list)[!names(return_list) %in% "id"]]
  }
  
  return(return_list)
}



