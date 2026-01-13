#' Coordinate the request and retrieval of OGC calls
#' 
#' @param args arguments from individual functions
#' @param output_id Name of id column to return
#' @param service Endpoint name.
#' 
#' @noRd
#' @return data.frame with attributes
get_ogc_data <- function(args,
                         output_id, 
                         service){

  args <- switch_arg_id(args, 
                        id_name = output_id, 
                        service = service)
  
  args <- check_limits(args)
  
  properties <- args[["properties"]]
  args[["properties"]] <- switch_properties_id(properties, 
                                               id = output_id)
  convertType <- args[["convertType"]] 
  args[["convertType"]] <- NULL
  args[["service"]] <- service
  
  req <- do.call(construct_api_requests, args)

  if("no_paging" %in% names(args)){
    no_paging <- args[["no_paging"]]
    args[["no_paging"]] <- NULL    
  } else {
    no_paging <- FALSE
  }
  
  message("Requesting:\n", req$url)
  
  if(no_paging){
    return_list <- get_csv(req, limit = args[["limit"]])
  } else {
    return_list <- walk_pages(req)
  }
  
  if(is.na(args[["skipGeometry"]])){
    skipGeometry <- FALSE
  } else {
    skipGeometry <- args[["skipGeometry"]]
  }
  
  return_list <- deal_with_empty(return_list, properties, service,
                                 skipGeometry, convertType, no_paging)
  
  return_list <- rejigger_cols(return_list, properties, output_id)
  
  if(convertType){
    return_list <- cleanup_cols(return_list, service)
    return_list <- order_results(return_list)

    # Mostly drop the id column except ts-meta, monitoring location:
    if(!service %in% c("monitoring-locations",
                       "time-series-metadata",
                       "parameter-codes")){
      return_list <- return_list[, names(return_list)[names(return_list)!= output_id]]
    }
    # Move other id columns:
    return_list <- move_id_col(return_list, 
                               output_id)
  }
  
  attr(return_list, "request") <- req
  attr(return_list, "queryTime") <- Sys.time()
  return_list
}

order_results <- function(df){
  
  if(all(c("time", "monitoring_location_id") %in% names(df))){
    df <- df[order(df$time, 
                   df$monitoring_location_id), ]
  } else if ("time" %in% names(df)) {
    df <- df[order(df$time), ]    
  }
  
  return(df)
}

move_id_col <- function(df, output_id){

  if("time_series_id" %in% names(df)){
    df <- df[, c(names(df)[names(df)!= "time_series_id"],
                 "time_series_id")]
  }
  
  if("field_visit_id" %in% names(df)){
    df <- df[, c(names(df)[names(df)!= "field_visit_id"],
                 "field_visit_id")]
  }

  return(df)
}

#' Switch properties id
#' 
#' If a user asks for either "id" or "output_id", it is only included in the 
#' properties if that's the only column requested. "id" will always come back,
#' so it is not needed in the properties call.
#' 
#' @noRd
#' @return list
#' @examples
#' 
#' properties <- c("id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties, 
#'                               id = "monitoring_location_id")
#'                               
#' properties2 <- c("monitoring_location_id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties2, 
#'                               id = "monitoring_location_id")
#'                               
#' properties3 <- c("monitoring_locations_id", "state_name", "country_name")
#' dataRetrieval:::switch_properties_id(properties3, 
#'                               id = "monitoring_location_id")
#'                               
#' properties4 <- c("monitoring_location_id")
#' dataRetrieval:::switch_properties_id(properties4, 
#'                               id = "monitoring_location_id")
#'                               
#' properties5 <- c("monitoring_location_id", "geometry")
#' dataRetrieval:::switch_properties_id(properties5, 
#'                               id = "monitoring_location_id")
#'
switch_properties_id <- function(properties, id){
  
  orig_properties <- properties
  if(!all(is.na(properties))){
    if("id" %in% properties){
      properties <- properties[properties != "id"]
    } else if (id %in% properties){
      properties <- properties[properties != id]
    }
    
    if("geometry" %in% properties){
      properties <- properties[properties != "geometry"]
    }
    
    if(length(properties) == 0){
      # If a user requested only id and/or geometry, properties would now be empty
      # geometry is taken care of with skipGeometry
      properties <- "id"
    }
  }
  
  return(properties)
}
