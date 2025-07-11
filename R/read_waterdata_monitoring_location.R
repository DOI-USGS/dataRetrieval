#' Get USGS Site Data
#' 
#' Description `r get_description("monitoring-locations")`
#' 
#' @export
#' @param monitoring_location_id `r get_params("monitoring-locations")$id`
#' @param agency_code `r get_params("monitoring-locations")$agency_code`
#' @param agency_name `r get_params("monitoring-locations")$agency_name`
#' @param monitoring_location_number `r get_params("monitoring-locations")$monitoring_location_number`
#' @param monitoring_location_name `r get_params("monitoring-locations")$monitoring_location_name`
#' @param district_code `r get_params("monitoring-locations")$district_code`
#' @param state_name `r get_params("monitoring-locations")$state_name`
#' @param county_code `r get_params("monitoring-locations")$county_code`
#' @param county_name `r get_params("monitoring-locations")$county_name`
#' @param country_code `r get_params("monitoring-locations")$country_code`
#' @param country_name `r get_params("monitoring-locations")$country_name`
#' @param state_code `r get_params("monitoring-locations")$state_code`
#' @param minor_civil_division_code `r get_params("monitoring-locations")$minor_civil_division_code`
#' @param site_type_code `r get_params("monitoring-locations")$site_type_code`
#' @param site_type `r get_params("monitoring-locations")$site_type`
#' @param hydrologic_unit_code `r get_params("monitoring-locations")$hydrologic_unit_code`
#' @param basin_code `r get_params("monitoring-locations")$basin_code`
#' @param altitude `r get_params("monitoring-locations")$altitude`
#' @param altitude_accuracy `r get_params("monitoring-locations")$altitude_accuracy`
#' @param altitude_method_code `r get_params("monitoring-locations")$altitude_method_code`
#' @param altitude_method_name `r get_params("monitoring-locations")$altitude_method_name`
#' @param vertical_datum `r get_params("monitoring-locations")$vertical_datum`
#' @param vertical_datum_name `r get_params("monitoring-locations")$vertical_datum_name`
#' @param horizontal_positional_accuracy_code `r get_params("monitoring-locations")$horizontal_positional_accuracy_code`
#' @param horizontal_positional_accuracy `r get_params("monitoring-locations")$horizontal_positional_accuracy`
#' @param horizontal_position_method_code `r get_params("monitoring-locations")$horizontal_position_method_code`
#' @param horizontal_position_method_name `r get_params("monitoring-locations")$horizontal_position_method_name`
#' @param original_horizontal_datum `r get_params("monitoring-locations")$original_horizontal_datum`
#' @param original_horizontal_datum_name `r get_params("monitoring-locations")$original_horizontal_datum_name`
#' @param drainage_area `r get_params("monitoring-locations")$drainage_area`
#' @param contributing_drainage_area `r get_params("monitoring-locations")$contributing_drainage_area`
#' @param time_zone_abbreviation `r get_params("monitoring-locations")$time_zone_abbreviation`
#' @param uses_daylight_savings `r get_params("monitoring-locations")$uses_daylight_savings`
#' @param construction_date `r get_params("monitoring-locations")$construction_date`
#' @param aquifer_code `r get_params("monitoring-locations")$aquifer_code`
#' @param national_aquifer_code `r get_params("monitoring-locations")$national_aquifer_code`
#' @param aquifer_type_code `r get_params("monitoring-locations")$aquifer_type_code`
#' @param well_constructed_depth `r get_params("monitoring-locations")$well_constructed_depth`
#' @param hole_constructed_depth `r get_params("monitoring-locations")$hole_constructed_depth`
#' @param depth_source_code `r get_params("monitoring-locations")$depth_source_code`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r schema <- check_OGC_requests(endpoint = "monitoring-locations", type = "schema"); paste(names(schema$properties), collapse = ", ")`.
#' @param bbox Only features that have a geometry that intersects the bounding
#' box are selected.The bounding box is provided as four or six numbers, depending
#' on whether the coordinate reference system includes a vertical axis (height or
#' depth). Coordinates are assumed to be in crs 4326. The expected format is a numeric 
#' vector structured: c(xmin,ymin,xmax,ymax). Another way to think of it is c(Western-most longitude,
#' Southern-most latitude, Eastern-most longitude, Northern-most longitude).
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 10000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param max_results The optional maximum number of rows to return. This value
#' must be less than the requested limit. 
#' @param skipGeometry This option can be used to skip response geometries for
#' each feature. The returning object will be a data frame with no spatial
#' information.
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' site_info <- read_waterdata_monitoring_location(monitoring_location_id = site)
#' 
#' site_slim <- read_waterdata_monitoring_location(
#'                              monitoring_location_id = c("USGS-05114000",
#'                                                         "USGS-09423350"),
#'                              properties = c("monitoring_location_id",
#'                                             "state_name",
#'                                             "country_name"))
#'                                             
#' site_slim_no_sf_slim <- read_waterdata_monitoring_location(monitoring_location_id = site,
#'                                            properties = c("monitoring_location_id",
#'                                                           "state_name",
#'                                                           "country_name"), 
#'                                            skipGeometry = TRUE)
#'
#' site_info_no_sf <- read_waterdata_monitoring_location(monitoring_location_id = site,
#'                                    skipGeometry = TRUE)
#' 
#' bbox_vals = c(-94.00, 35.0, -93.5, 35.5)
#' multi_site <- read_waterdata_monitoring_location(bbox = bbox_vals)
#' multi_site_n_100 <- read_waterdata_monitoring_location(bbox = bbox_vals,
#'                                                   max_results = 100)
#' multi_site_limit_100 <- read_waterdata_monitoring_location(bbox = bbox_vals,
#'                                                       limit = 100)
#' }
read_waterdata_monitoring_location <- function(monitoring_location_id = NA_character_,
                            agency_code = NA_character_,
                            agency_name = NA_character_,
                            monitoring_location_number = NA_character_,
                            monitoring_location_name = NA_character_,
                            district_code = NA_character_,
                            country_code = NA_character_,
                            country_name = NA_character_,
                            state_code = NA_character_,
                            state_name = NA_character_,
                            county_code = NA_character_,
                            county_name = NA_character_,
                            minor_civil_division_code = NA_character_,
                            site_type_code = NA_character_,
                            site_type = NA_character_,
                            hydrologic_unit_code = NA_character_,
                            basin_code = NA_character_,
                            altitude = NA_character_,
                            altitude_accuracy = NA_character_,
                            altitude_method_code = NA_character_,
                            altitude_method_name = NA_character_,              
                            vertical_datum = NA_character_,
                            vertical_datum_name = NA_character_,
                            horizontal_positional_accuracy_code = NA_character_,
                            horizontal_positional_accuracy = NA_character_,
                            horizontal_position_method_code = NA_character_,
                            horizontal_position_method_name = NA_character_,
                            original_horizontal_datum = NA_character_,
                            original_horizontal_datum_name = NA_character_,
                            drainage_area = NA_character_,
                            contributing_drainage_area = NA_character_,    
                            time_zone_abbreviation = NA_character_,
                            uses_daylight_savings = NA_character_,
                            construction_date = NA_character_,
                            aquifer_code = NA_character_,
                            national_aquifer_code = NA_character_,
                            aquifer_type_code = NA_character_,
                            well_constructed_depth = NA_character_,
                            hole_constructed_depth = NA_character_,
                            depth_source_code = NA_character_,
                            properties = NA_character_,
                            bbox = NA,
                            limit = NA,
                            max_results = NA,
                            skipGeometry = NA){

  service <- "monitoring-locations"
  output_id <- "monitoring_location_id"
  
  args <- mget(names(formals()))
  args[["convertType"]] <- FALSE
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)

  return(return_list)
}
