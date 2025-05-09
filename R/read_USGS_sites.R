#' Get USGS Daily Data
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
#' @param properties The properties that should be included for each feature.
#' The parameter value is a comma-separated list of property names. Available options are
#' `r schema <- check_OGC_requests(endpoint = "monitoring-locations", type = "schema"); paste(names(schema$properties), collapse = ", ")`
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
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' site <- "USGS-02238500"
#' site_info <- read_USGS_sites(monitoring_location_id = site)
#' 
#' site_slim <- read_USGS_sites(monitoring_location_id = site,
#'                              properties = c("monitoring_locations_id",
#'                                             "state_name",
#'                                             "country_name"))
#'
#' site_info_no_sf <- read_USGS_sites(monitoring_location_id = site,
#'                                    skipGeometry = TRUE)
#' 
#' multi_site <- read_USGS_sites(state_name = "Wisconsin")
#' 
#' }
read_USGS_sites <- function(monitoring_location_id = NA_character_,
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
                            limit = 10000,
                            crs = NA_character_,
                            skipGeometry = NA){
  
  message("Function in development, use at your own risk.")
  
  service <- "monitoring-locations"
  
  site_req <- construct_api_requests(service = service,
                                     id = monitoring_location_id,
                                     agency_code = agency_code,
                                     agency_name = agency_name,
                                     monitoring_location_number = monitoring_location_number,
                                     monitoring_location_name = monitoring_location_name,
                                     district_code = district_code,
                                     country_code = country_code,
                                     country_name = country_name,
                                     state_code = state_code,
                                     state_name = state_name,
                                     county_code = county_code,
                                     county_name = county_name,
                                     minor_civil_division_code = minor_civil_division_code,
                                     site_type_code = site_type_code,
                                     site_type = site_type,
                                     hydrologic_unit_code = hydrologic_unit_code,
                                     basin_code = basin_code,
                                     altitude = altitude,
                                     altitude_accuracy = altitude_accuracy,
                                     altitude_method_code = altitude_method_code,
                                     altitude_method_name = altitude_method_name,              
                                     vertical_datum = vertical_datum,
                                     vertical_datum_name = vertical_datum_name,
                                     horizontal_positional_accuracy_code = horizontal_positional_accuracy_code,
                                     horizontal_positional_accuracy = horizontal_positional_accuracy,
                                     horizontal_position_method_code = horizontal_position_method_code,
                                     horizontal_position_method_name = horizontal_position_method_name,
                                     original_horizontal_datum = original_horizontal_datum,
                                     original_horizontal_datum_name = original_horizontal_datum_name,
                                     drainage_area = drainage_area,
                                     contributing_drainage_area = contributing_drainage_area,    
                                     time_zone_abbreviation = time_zone_abbreviation,
                                     uses_daylight_savings = uses_daylight_savings,
                                     construction_date = construction_date,
                                     aquifer_code = aquifer_code,
                                     national_aquifer_code = national_aquifer_code,
                                     aquifer_type_code = aquifer_type_code,
                                     well_constructed_depth = well_constructed_depth,
                                     hole_constructed_depth = hole_constructed_depth,
                                     depth_source_code = depth_source_code,
                                     limit = limit,
                                     bbox = bbox,
                                     crs = crs,
                                     skipGeometry = skipGeometry,
                                     properties = properties)
  
  return_list <- walk_pages(site_req)
  
  return_list <- rejigger_cols(return_list, properties, service)
  
  return(return_list)
}
