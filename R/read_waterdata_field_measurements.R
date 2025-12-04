#' Get USGS Field Measurement Water Data
#' 
#' @description `r get_description("field-measurements")`
#' 
#' @export
#' @param monitoring_location_id `r get_params("field-measurements")$monitoring_location_id`
#' @param parameter_code `r get_params("field-measurements")$parameter_code`
#' @param observing_procedure_code `r get_params("field-measurements")$observing_procedure_code`
#' @param time `r get_params("field-measurements")$time`
#' @param value `r get_params("field-measurements")$value`
#' @param unit_of_measure `r get_params("field-measurements")$unit_of_measure`
#' @param approval_status `r get_params("field-measurements")$approval_status`
#' @param last_modified `r get_params("field-measurements")$last_modified`
#' @param qualifier `r get_params("field-measurements")$qualifier`
#' @param field_visit_id `r get_params("field-measurements")$field_visit_id`
#' @param observing_procedure `r get_params("field-measurements")$observing_procedure`
#' @param vertical_datum `r get_params("field-measurements")$vertical_datum`
#' @param measuring_agency `r get_params("field-measurements")$measuring_agency`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r schema <- check_OGC_requests(endpoint = "field-measurements", type = "schema"); paste(names(schema$properties)[!names(schema$properties) %in% c("id")], collapse = ", ")`
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
#' @param max_results The optional maximum number of rows to return. This value
#' must be less than the requested limit. 
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @param convertType logical, defaults to `TRUE`. If `TRUE`, the function
#' will convert the data to dates and qualifier to string vector.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' field_data_sf <- read_waterdata_field_measurements(monitoring_location_id = site)
#'
#' groundwater <- read_waterdata_field_measurements(monitoring_location_id = "USGS-375907091432201")
#'
#' gwl_data <- read_waterdata_field_measurements(monitoring_location_id = "USGS-375907091432201",
#'                            parameter_code = "72019",
#'                            skipGeometry = TRUE)
#'                         
#' gwl_data_period <- read_waterdata_field_measurements(
#'                                   monitoring_location_id = "USGS-375907091432201",
#'                                   parameter_code = "72019",
#'                                   time = "P20Y")
#' 
#' multi_site <- read_waterdata_field_measurements(
#'                               monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-263819081585801"),
#'                               parameter_code = c("62611", "72019"))
#'                               
#' old_df <- read_waterdata_field_measurements(monitoring_location_id = "USGS-425957088141001", 
#'                                               time = c("1980-01-01", NA))
#'                                               
#' surface_water <- read_waterdata_field_measurements(
#'                          monitoring_location_id = c("USGS-07069000",
#'                                                     "USGS-07064000",
#'                                                     "USGS-07068000"),
#'                          time = "2024-07-01T00:00:00Z/..",
#'                          parameter_code = "00060")
#' 
#' 
#' }
read_waterdata_field_measurements <- function(monitoring_location_id = NA_character_,
                                             parameter_code = NA_character_,
                                             observing_procedure_code = NA_character_,
                                             properties = NA_character_,
                                             field_visit_id = NA_character_,
                                             approval_status = NA_character_,
                                             unit_of_measure = NA_character_,
                                             qualifier = NA_character_,
                                             value = NA,
                                             last_modified = NA_character_,
                                             observing_procedure = NA_character_,
                                             vertical_datum = NA_character_,
                                             measuring_agency = NA_character_,
                                             skipGeometry = NA,
                                             time = NA_character_,
                                             bbox = NA,
                                             limit = NA,
                                             max_results = NA,
                                             convertType = TRUE){
  
  service <- "field-measurements"
  output_id <- "field_measurement_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  if(convertType){
    return_list <- order_results(return_list, properties)
    return_list <- return_list[,names(return_list)[names(return_list)!= output_id]]
    if("field_visit_id" %in% names(return_list)){
      return_list <- return_list[, c( names(return_list)[names(return_list)!= "field_visit_id"],
                                      "field_visit_id")]
    }
  }
  
  return(return_list)
}



