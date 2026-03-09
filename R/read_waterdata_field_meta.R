#' Get USGS Field Measurement Metadata
#' 
#' @description `r get_description("field-measurements-metadata")`
#' 
#' @export
#' @param monitoring_location_id `r get_ogc_params("field-measurements-metadata")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param parameter_code `r get_ogc_params("field-measurements-metadata")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param parameter_name `r get_ogc_params("field-measurements-metadata")$parameter_name`
#' Multiple parameter_names can be requested as a character vector.
#' @param parameter_description `r get_ogc_params("field-measurements-metadata")$parameter_description`
#' Multiple parameter_descriptions can be requested as a character vector.
#' @param last_modified `r get_ogc_params("field-measurements-metadata")$last_modified`
#' 
#' See also Details below for more information.
#' @param begin `r get_ogc_params("field-measurements-metadata")$begin`
#' 
#' See also Details below for more information.
#' @param end `r get_ogc_params("field-measurements-metadata")$end`
#' 
#' See also Details below for more information.
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("field-measurements-metadata", "field_measurement_id")`.
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
#' 
#' @inherit read_waterdata_continuous details
#' 
#' 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' field_data_sf <- read_waterdata_field_meta(monitoring_location_id = site)
#'
#' groundwater <- read_waterdata_field_meta(monitoring_location_id = "USGS-375907091432201")
#'
#' gwl_data <- read_waterdata_field_meta(monitoring_location_id = "USGS-02238500",
#'                            parameter_code = "00060",
#'                            begin = as.POSIXct(c(NA,
#'                                                "2025-08-27 12:00:00"),
#'                                              tz = "America/Chicago"),
#'                            skipGeometry = TRUE)
#'                         
#' gwl_data_period <- read_waterdata_field_meta(
#'                                   monitoring_location_id = "USGS-375907091432201",
#'                                   parameter_code = "72019",
#'                                   last_modified = "P1Y")
#' 
#' multi_site <- read_waterdata_field_meta(
#'                               monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-263819081585801"),
#'                               parameter_code = c("62611", "72019"))
#'
#'                                               
#' surface_water <- read_waterdata_field_meta(
#'                          monitoring_location_id = c("USGS-07069000",
#'                                                     "USGS-07064000",
#'                                                     "USGS-07068000"),
#'                          end = "2024-07-01T00:00:00Z/..",
#'                          parameter_code = "00060")
#' 
#' 
#' }
read_waterdata_field_meta <- function(monitoring_location_id = NA_character_,
                                      parameter_code = NA_character_,
                                      parameter_name = NA_character_,
                                      parameter_description = NA_character_,
                                      begin = NA_character_,
                                      end = NA_character_,
                                      last_modified = NA_character_,
                                      properties = NA_character_,
                                      skipGeometry = NA,
                                      bbox = NA,
                                      limit = NA,
                                      convertType = TRUE,
                                      no_paging = FALSE){
  
  service <- "field-measurements-metadata"
  output_id <- "field_series_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  return(return_list)
}

