#' Get USGS Monitoring Location Metadata
#' 
#' @description `r get_description("combined-metadata")`
#' 
#' @export
#' @param monitoring_location_id `r get_ogc_params("combined-metadata")$monitoring_location_id`
#' Multiple monitoring_location_ids can be requested as a character vector.
#' @param agency_code `r get_ogc_params("combined-metadata")$agency_code`
#' @param agency_name `r get_ogc_params("combined-metadata")$agency_name`
#' @param monitoring_location_number `r get_ogc_params("combined-metadata")$monitoring_location_number`
#' @param monitoring_location_name `r get_ogc_params("combined-metadata")$monitoring_location_name`
#' @param district_code `r get_ogc_params("combined-metadata")$district_code`
#' @param state_name `r get_ogc_params("combined-metadata")$state_name`
#' @param county_code `r get_ogc_params("combined-metadata")$county_code`
#' @param county_name `r get_ogc_params("combined-metadata")$county_name`
#' @param country_code `r get_ogc_params("combined-metadata")$country_code`
#' @param country_name `r get_ogc_params("combined-metadata")$country_name`
#' @param state_code `r get_ogc_params("combined-metadata")$state_code`
#' @param minor_civil_division_code `r get_ogc_params("combined-metadata")$minor_civil_division_code`
#' @param site_type_code `r get_ogc_params("combined-metadata")$site_type_code`
#' @param site_type `r get_ogc_params("combined-metadata")$site_type`
#' @param hydrologic_unit_code `r get_ogc_params("combined-metadata")$hydrologic_unit_code`
#' @param basin_code `r get_ogc_params("combined-metadata")$basin_code`
#' @param altitude `r get_ogc_params("combined-metadata")$altitude`
#' @param altitude_accuracy `r get_ogc_params("combined-metadata")$altitude_accuracy`
#' @param altitude_method_code `r get_ogc_params("combined-metadata")$altitude_method_code`
#' @param altitude_method_name `r get_ogc_params("combined-metadata")$altitude_method_name`
#' @param vertical_datum `r get_ogc_params("combined-metadata")$vertical_datum`
#' @param vertical_datum_name `r get_ogc_params("combined-metadata")$vertical_datum_name`
#' @param horizontal_positional_accuracy_code `r get_ogc_params("combined-metadata")$horizontal_positional_accuracy_code`
#' @param horizontal_positional_accuracy `r get_ogc_params("combined-metadata")$horizontal_positional_accuracy`
#' @param horizontal_position_method_code `r get_ogc_params("combined-metadata")$horizontal_position_method_code`
#' @param horizontal_position_method_name `r get_ogc_params("combined-metadata")$horizontal_position_method_name`
#' @param original_horizontal_datum `r get_ogc_params("combined-metadata")$original_horizontal_datum`
#' @param original_horizontal_datum_name `r get_ogc_params("combined-metadata")$original_horizontal_datum_name`
#' @param drainage_area `r get_ogc_params("combined-metadata")$drainage_area`
#' @param contributing_drainage_area `r get_ogc_params("combined-metadata")$contributing_drainage_area`
#' @param time_zone_abbreviation `r get_ogc_params("combined-metadata")$time_zone_abbreviation`
#' @param uses_daylight_savings `r get_ogc_params("combined-metadata")$uses_daylight_savings`
#' @param construction_date `r get_ogc_params("combined-metadata")$construction_date`
#' @param aquifer_code `r get_ogc_params("combined-metadata")$aquifer_code`
#' @param national_aquifer_code `r get_ogc_params("combined-metadata")$national_aquifer_code`
#' @param aquifer_type_code `r get_ogc_params("combined-metadata")$aquifer_type_code`
#' @param well_constructed_depth `r get_ogc_params("combined-metadata")$well_constructed_depth`
#' @param hole_constructed_depth `r get_ogc_params("combined-metadata")$hole_constructed_depth`
#' @param depth_source_code `r get_ogc_params("combined-metadata")$depth_source_code`
#' @param thresholds `r get_ogc_params("combined-metadata")$thresholds`
#' @param parameter_code `r get_ogc_params("combined-metadata")$parameter_code`
#' Multiple parameter_codes can be requested as a character vector.
#' @param parameter_name `r get_ogc_params("combined-metadata")$parameter_name`
#' Multiple parameter_names can be requested as a character vector.
#' @param unit_of_measure `r get_ogc_params("combined-metadata")$unit_of_measure`
#' @param parameter_description `r get_ogc_params("combined-metadata")$parameter_description`
#' @param data_type `r get_ogc_params("combined-metadata")$data_type`
#' @param primary `r get_ogc_params("combined-metadata")$primary`
#' @param web_description `r get_ogc_params("combined-metadata")$web_description`
#' @param parent_time_series_id `r get_ogc_params("combined-metadata")$parent_time_series_id`
#' @param statistic_id `r get_ogc_params("combined-metadata")$statistic_id`
#' Multiple statistic_ids can be requested as a character vector.
#' @param computation_identifier `r get_ogc_params("combined-metadata")$computation_identifier`
#' Multiple computation_identifiers can be requested as a character vector.
#' @param computation_period_identifier `r get_ogc_params("combined-metadata")$computation_period_identifier`
#' Multiple computation_period_identifiers can be requested as a character vector.
#' @param sublocation_identifier `r get_ogc_params("combined-metadata")$sublocation_identifier`
#' @param last_modified `r get_ogc_params("combined-metadata")$last_modified`
#' 
#' See also Details below for more information.
#' @param begin `r get_ogc_params("combined-metadata")$begin`
#' 
#' See also Details below for more information.
#' @param end `r get_ogc_params("combined-metadata")$end`
#' 
#' See also Details below for more information.
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r dataRetrieval:::get_properties_for_docs("combined-metadata", "field_measurement_id")`.
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
#' site <- "USGS-05407000"
#' available_data_sf <- read_waterdata_combined_meta(monitoring_location_id = site)
#'
#' groundwater <- read_waterdata_combined_meta(monitoring_location_id = "USGS-375907091432201")
#' 
#' date_wi_data <- read_waterdata_combined_meta(state_name = "Wisconsin",
#'                                         county_name = "Dane County")
#'
#' 
#' multi_site <- read_waterdata_combined_meta(
#'                               monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-263819081585801"),
#'                               parameter_code = c("62611", "72019"))
#'
#'                                               
#' surface_water <- read_waterdata_combined_meta(
#'                          monitoring_location_id = c("USGS-07069000",
#'                                                     "USGS-07064000",
#'                                                     "USGS-07068000"),
#'                          end = "P1M",
#'                          parameter_code = "00060")
#' 
#' hucs <- read_waterdata_combined_meta(
#'                          hydrologic_unit_code = c("11010008", "11010009"),
#'                          site_type = c("Stream", "Spring")
#' )
#' }
read_waterdata_combined_meta <- function(monitoring_location_id = NA_character_,
                                      parameter_code = NA_character_,
                                      parameter_name = NA_character_,
                                      unit_of_measure = NA_character_,
                                      statistic_id = NA_character_,
                                      parameter_description = NA_character_,
                                      data_type = NA_character_,
                                      computation_identifier = NA_character_,
                                      computation_period_identifier = NA_character_,
                                      thresholds = NA_character_,
                                      sublocation_identifier = NA_character_,
                                      primary = NA_character_,
                                      web_description = NA_character_,
                                      parent_time_series_id = NA_character_,
                                      begin = NA_character_,
                                      end = NA_character_,
                                      last_modified = NA_character_,
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
                                      skipGeometry = NA,
                                      bbox = NA,
                                      limit = NA,
                                      convertType = TRUE,
                                      no_paging = FALSE){
  
  service <- "combined-metadata"
  output_id <- "combined_meta_id"
  
  args <- mget(names(formals()))
  return_list <- get_ogc_data(args,
                              output_id, 
                              service)
  
  return(return_list)
}

