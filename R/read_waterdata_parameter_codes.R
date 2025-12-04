#' Get USGS Parameter Code Information
#' 
#' @description `r get_description("parameter-codes")`
#' 
#' @export
#' @param parameter_code `r get_params("parameter-codes")$id`
#' @param parameter_name `r get_params("parameter-codes")$parameter_name`
#' @param unit_of_measure `r get_params("parameter-codes")$unit_of_measure`
#' @param parameter_group_code `r get_params("parameter-codes")$parameter_group_code`
#' @param parameter_description `r get_params("parameter-codes")$parameter_description`
#' @param medium `r get_params("parameter-codes")$medium`
#' @param statistical_basis `r get_params("parameter-codes")$statistical_basis`
#' @param weight_basis `r get_params("parameter-codes")$weight_basis`
#' @param sample_fraction `r get_params("parameter-codes")$sample_fraction`
#' @param temperature_basis `r get_params("parameter-codes")$temperature_basis`
#' @param epa_equivalence `r get_params("parameter-codes")$epa_equivalence`
#' @param properties A vector of requested columns to be returned from the query.
#' Available options are: 
#' `r schema <- check_OGC_requests(endpoint = "parameter-codes", type = "schema"); paste(names(schema$properties), collapse = ", ")`.
#' @param limit The optional limit parameter is used to control the subset of the 
#' selected features that should be returned in each page. The maximum allowable
#' limit is 50000. It may be beneficial to set this number lower if your internet
#' connection is spotty. The default (`NA`) will set the limit to the maximum
#' allowable limit for the service.
#' @param max_results The optional maximum number of rows to return. This value
#' must be less than the requested limit. 
#' @examplesIf is_dataRetrieval_user()
#' 
#' \donttest{
#' pcode <- "00060"
#' pcode_info <- read_waterdata_parameter_codes(parameter_code = pcode)
#' 
#' pcodes <- read_waterdata_parameter_codes(parameter_code = c("00660", "00060"))
#' 
#' # equivalent to read_waterdata_metadata("parameter-codes")
#' all_pcodes <- read_waterdata_parameter_codes() 
#' 
#' total_nutrients <- read_waterdata_parameter_codes(parameter_group_code = "NUT",
#'                                            sample_fraction = "Total")
#'                                            
#' group_of_nutrients <- read_waterdata_parameter_codes(parameter_group_code = "NUT",
#'                                                     unit_of_measure = c("mg/l",
#'                                                     "mg/l as N", "mg/l NO3",
#'                                                     "mg/l asNO2"))
#' 
#' }
read_waterdata_parameter_codes <- function(parameter_code = NA_character_,
                                          parameter_name = NA_character_,
                                          unit_of_measure = NA_character_,
                                          parameter_group_code = NA_character_,
                                          parameter_description = NA_character_,
                                          medium = NA_character_,
                                          statistical_basis = NA_character_,
                                          weight_basis = NA_character_,
                                          sample_fraction = NA_character_,
                                          temperature_basis = NA_character_,
                                          epa_equivalence = NA_character_,
                                          properties = NA_character_,
                                          limit = NA,
                                          max_results = NA){

  service <- "parameter-codes"
  output_id <- "parameter_code"
  
  args <- mget(names(formals()))
  args[["convertType"]] <- FALSE
  args[["skipGeometry"]] <- TRUE
  args[["bbox"]] <- NA
  
  if(all(lengths(args) == 1)){
    return_list <- suppressWarnings(get_ogc_data(args = args,
                                                 output_id = output_id,
                                                 service =  service))
  } else {
    
    message("Current API functionality requires pulling the full parameter-codes list.
It is expected that updates to the API will eliminate this need, but in the meantime
consider running read_waterdata_parameter_code() with no query parameters, and filtering
in a post-processing step.")
    
    return_list <- read_waterdata_metadata(collection = service, 
                                 max_results = max_results,
                                 limit = limit)
    args[["convertType"]] <- NULL
    args[["skipGeometry"]] <- NULL
    args_to_filter <- args[!is.na(args)]
    for(param in names(args_to_filter)){
      return_list <- return_list[return_list[[param]] %in% args_to_filter[[param]],]
    }
  }

  return(return_list)
}

