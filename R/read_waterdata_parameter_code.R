#' Get USGS Daily Data
#' 
#' Description `r get_description("parameter-codes")`
#' 
#' @export
#' @param parameter_code `r get_params("parameter-codes")$id`
#' @param parameter_name `r get_params("parameter-codes")$parameter_name`
#' @param unit_of_measure `r get_params("parameter-codes")$unit_of_measure`
#' @param parameter_group_code `r get_params("parameter-codes")$parameter_group_code`
#' @param parameter_description `r get_params("parameter-codes")$parameter_description`
#' @param medium `r get_params("parameter-codes")$medium`
#' @param statistical_basis `r get_params("parameter-codes")$statistical_basis`
#' @param time_basis `r get_params("parameter-codes")$time_basis`
#' @param weight_basis `r get_params("parameter-codes")$weight_basis`
#' @param particle_size_basis `r get_params("parameter-codes")$particle_size_basis`
#' @param sample_fraction `r get_params("parameter-codes")$sample_fraction`
#' @param temperature_basis `r get_params("parameter-codes")$temperature_basis`
#' @param epa_equivalence `r get_params("parameter-codes")$epa_equivalence`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r schema <- check_OGC_requests(endpoint = "parameter-codes", type = "schema"); paste(names(schema$properties), collapse = ", ")`
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 10000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param max_results The optional maximum number of rows to return. This value
#' must be less than the requested limit. 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' pcode <- "72019"
#' pcode_info <- read_waterdata_parameter_code(parameter_code = pcode)
#'
#' uv_data_trim <- read_waterdata_parameter_code(monitoring_location_id = site,
#'                           parameter_code = pcode, 
#'                           properties = c("monitoring_location_id",
#'                                          "value",
#'                                          "time"))
#'                         
#' uv_data_period <- read_waterdata_parameter_code(monitoring_location_id = site,
#'                                   parameter_code = pcode,
#'                                   time = "P7D")
#' 
#' multi_site <- read_waterdata_parameter_code(monitoring_location_id =  c("USGS-451605097071701",
#'                                                           "USGS-14181500"),
#'                               parameter_code = c("00060", "72019"))
#' 
#' }
read_waterdata_parameter_code <- function(parameter_code = NA_character_,
                                          parameter_name = NA_character_,
                                          unit_of_measure = NA_character_,
                                          parameter_group_code = NA_character_,
                                          parameter_description = NA_character_,
                                          medium = NA_character_,
                                          statistical_basis = NA_character_,
                                          time_basis = NA_character_,
                                          weight_basis = NA_character_,
                                          particle_size_basis = NA,
                                          sample_fraction = NA_character_,
                                          temperature_basis = NA,
                                          epa_equivalence = NA_character_,
                                          properties = NA_character_,
                                          limit = NA,
                                          max_results = NA){
  
  service <- "parameter-codes"
  output_id <- "parameter_code"
  
  args <- mget(names(formals()))
  args[["service"]] <-  service
  args[["skipGeometry"]] <- TRUE
  args <- switch_arg_id(args, 
                        id_name = output_id, 
                        service = service)
  
  args[["properties"]] <- switch_properties_id(properties, 
                                               id_name = output_id, 
                                               service = service)
  
  args[["convertType"]] <- NULL
  
  pcode_req <- do.call(construct_api_requests, args)
  
  return_list <- walk_pages(pcode_req, max_results)
  
  return_list <- deal_with_empty(return_list, properties, service)

  return_list <- rejigger_cols(return_list, properties, output_id)

  return(return_list)
}



