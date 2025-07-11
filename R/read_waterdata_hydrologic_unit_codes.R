#' Get USGS Hydrologic Unit Code Information
#' 
#' Description `r get_description("hydrologic-unit-codes")`
#' 
#' @export
#' @param hydrologic_unit_code `r get_params("hydrologic-unit-codes")$id`
#' @param hydrologic_unit_name `r get_params("hydrologic-unit-codes")$hydrologic_unit_name`
#' @param hydrologic_unit_classification_code `r get_params("hydrologic-unit-codes")$hydrologic_unit_classification_code`
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
#' huc <- "0101000"
#' huc_info <- read_waterdata_hydrologic_unit_codes(hydrologic_unit_code = huc)
#' 
#' hucs_2 <- read_waterdata_hydrologic_unit_codes(hydrologic_unit_code = c("010100", "010100041404"))
#' 
#' all_hucs <- read_waterdata_hydrologic_unit_codes(max_results = 1000)
#' 
#' }
read_waterdata_hydrologic_unit_codes <- function(hydrologic_unit_code = NA_character_,
                                                 hydrologic_unit_name = NA_character_,
                                                 hydrologic_unit_classification_code = NA_character_,
                                                 limit = NA,
                                                 max_results = NA){

  service <- "hydrologic-unit-codes"
  output_id <- "hydrologic_unit_code"
  
  args <- mget(names(formals()))
  args[["convertType"]] <- FALSE
  args[["skipGeometry"]] <- TRUE
  args[["bbox"]] <- NA
  
  if(all(lengths(args) == 1)){
    return_list <- suppressWarnings(get_ogc_data(args = args,
                                                 output_id = output_id,
                                                 service =  service))
  } else {
    
    message("Current API functionality requires pulling the full hydrologic-unit-codes list.
It is expected that updates to the API will eliminate this need, but in the meantime
consider running read_waterdata_hydrologic_unit_codes() with no query parameters, and filtering
in a post-processing step.")
    
    return_list <- read_metadata(collection = service, 
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

