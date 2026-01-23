#' Return a data frame if there's an empty response
#' 
#' @param return_list data frame returned from walk_pages
#' @param properties A vector of requested columns
#' @param service character, can be any existing collection such
#' as "daily", "monitoring-locations", "time-series-metadata"
#' @param skipGeometry A logical for whether to return geometry
#' @param convertType A logical for whether to convert value to numeric
#' 
#' @return data.frame
#' @noRd
#' @examples
#' 
#' df <- dataRetrieval:::deal_with_empty(data.frame(NULL), 
#'                                       properties = c("time", "value"),
#'                                       service = "daily")
#'                                       
#' df2 <- dataRetrieval:::deal_with_empty(data.frame(NULL), 
#'                                       properties = NA,
#'                                       service = "daily")
#' 
deal_with_empty <- function(return_list, 
                            properties,
                            service, 
                            skipGeometry,
                            convertType,
                            no_paging = FALSE){
  
  if(nrow(return_list) == 0){
    
    if(all(is.na(properties))){
      schema <- check_OGC_requests(endpoint = service, type = "schema")
      properties <- names(schema$properties)
    }
    return_list <- data.frame(matrix(nrow = 0, 
                                     ncol = length(properties)))
    return_list <- lapply(return_list, as.character)
    names(return_list) <- properties
    
    single_params <- c("datetime", "last_modified", "begin", "end", "time")
    
    for(i in single_params){
      if(i %in% names(return_list)){
        return_list[[i]] <- as.POSIXct(as.character(), origin = "1970-01-01")
      }
    }
    
    if(convertType && service == "daily"){
      return_list$time <- as.Date(as.character())
    }
    
    if(convertType && "value" %in% names(return_list)){
      return_list$value <- as.numeric()
    }
    
    if(convertType && "contributing_drainage_area" %in% names(return_list)){
      return_list$contributing_drainage_area <- as.numeric()
    }
    
    return_list <- data.frame(return_list)
    return_list$geometry <- NULL
    
    if(!skipGeometry){
      if(!no_paging){
        return_list <- sf::st_as_sf(return_list, geometry = sf::st_sfc())
      } else {
        return_list$x <- numeric()
        return_list$y <- numeric()
      }
    } 
    
  }
  
  return(return_list)
}