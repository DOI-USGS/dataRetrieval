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
deal_with_empty <- function(return_list, properties, service, 
                            skipGeometry = TRUE,
                            convertType = TRUE){
  
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
      return_list <- sf::st_as_sf(return_list, geometry = sf::st_sfc())
    }
    
  }

  return(return_list)
}

#' Rejigger and rename  
#' 
#' Reorder columns based on users property input.
#' Add "service" prefix to returned "id" column.
#' This allows better integration with other endpoints.
#' 
#' @param df data frame returned from walk_pages
#' @param properties A vector of requested columns
#' @param service character, can be any existing collection such
#' as "daily", "monitoring-locations", "time-series-metadata"
#' 
#' @return data.frame
#' @noRd
#' @examples
#' 
#' df <- dataRetrieval:::deal_with_empty(data.frame(NULL), 
#'                                       properties = c("state_code", "county_code", "id"),
#'                                       service = "monitoring-locations")  
#' df2 <- dataRetrieval:::rejigger_cols(df, 
#'                                      properties = c("state_code", "id", "county_code"),
#'                                      output_id = "monitoring_location_id")
#'                                      
#' df3 <- dataRetrieval:::rejigger_cols(df, 
#'                                      properties = c("state_code", "monitoring_location_id", "county_code"),
#'                                      output_id = "monitoring_location_id")
#' 
rejigger_cols <- function(df, properties, output_id){

  if(!all(is.na(properties))){
    if(!"id" %in% properties){
      if(output_id %in% properties){
        names(df)[(names(df) == "id")] <- output_id
      } else {
        # just in case users become aware of the singular/plural issue
        # where the endpoint name is plural, but input to other endpoints are singular
        plural <- gsub("_id", "s_id", output_id)
        if(plural %in% properties){
          names(df)[(names(df) == "id")] <- plural
        }
      }
    }
    df <- df[, properties]
  } else {
    names(df)[(names(df) == "id")] <- output_id
  }
  df
}


#' Convert columns if needed
#' 
#' These are columns that have caused problems in testing.
#' Mostly if the columns are empty on 1 page, but not the next.
#' The qualifier column also comes back as a list column. This
#' is fine for many, but others prefer a character column.
#' 
#' 
#' @param df data frame returned from walk_pages
#' @param service character, can be any existing collection such
#' as "daily"
#' @return data.frame
#' @noRd
#' @examples
#' 
#' df <- dataRetrieval:::deal_with_empty(data.frame(NULL), 
#'                                       properties = c("time", "value", "id", "qualifier"),
#'                                       service = "daily")
#' df2 <- dataRetrieval:::rejigger_cols(df, 
#'                                      properties = c("value", "id", "time", "qualifier"),
#'                                      service = "daily")
#' df3 <- dataRetrieval:::cleanup_cols(df2)
#' 
cleanup_cols <- function(df, service = "daily"){
  
  if("qualifier" %in% names(df)){
    if(!all(is.na(df$qualifier))){
      df$qualifier <- vapply(X = df$qualifier,
                             FUN = function(x) paste(x, collapse = ", "),
                             FUN.VALUE =  c(NA_character_)) 
    }
  }
  
  if("time" %in% names(df)){
    if(service == "daily"){
      df$time <- as.Date(df$time)
    }
    # leave some room here for POSIXct in the other services.
  }
  
  if("value" %in% names(df)){
    df$value <- as.numeric(df$value)
  }
  
  if("contributing_drainage_area" %in% names(df)){
    df$contributing_drainage_area <- as.numeric(df$contributing_drainage_area)
  }  
  
  df
}

#' Next request URL
#' 
#' Custom function to find the "next" URL from the API response.
#' @seealso [httr2::req_perform_iterative]
#' 
#' @param resp httr2 response from last request
#' @param req httr2 request from last time
#' 
#' @noRd
#' @return the url for the next request
#' 
next_req_url <- function(resp, req) {

  body <- httr2::resp_body_json(resp)
  
  if(isTRUE(body[["numberReturned"]] == 0)){
    return(NULL)
  }
  
  header_info <- httr2::resp_headers(resp)
  if(Sys.getenv("API_USGS_PAT") != ""){
    message("Remaining requests this hour:", header_info$`x-ratelimit-remaining`, " ")
  }
  
  links <- body$links
  if(any(sapply(links, function(x) x$rel) == "next")){
    next_index <- which(sapply(links, function(x) x$rel) == "next")
    
    next_url <- links[[next_index]][["href"]]

    return(httr2::req_url(req = req, url = next_url))
  } else {
    return(NULL)
  }
}

#' Get single response data frame
#' 
#' Depending on skipGeometry to decide to use sf or not.
#' 
#' @noRd
#' 
#' @param resp httr2 response from last request
#' 
#' @return data.frame
#' 
get_resp_data <- function(resp) {
  
  body <- httr2::resp_body_json(resp)
  
  if(isTRUE(body[["numberReturned"]] == 0)){
    return(data.frame())
  }
  
  use_sf <- !grepl("skipGeometry=true", resp$url, ignore.case = TRUE)
  return_df <- sf::read_sf(httr2::resp_body_string(resp))
  
  if(!use_sf){
    return_df <- sf::st_drop_geometry(return_df)
    if("AsGeoJSON(geometry)" %in% names(return_df)){
      return_df <- return_df[, !names(return_df) %in% "AsGeoJSON(geometry)"]
    }
  } 

  return(return_df)
  
}

#' Walk through the pages
#' 
#' @param req httr2 initial request
#' 
#' @noRd
#' @return data.frame with attributes
walk_pages <- function(req, max_results){
  
  message("Requesting:\n", req$url)
  
  if(is.na(max_results)){
    resps <- httr2::req_perform_iterative(req, 
                                          next_req = next_req_url, 
                                          max_reqs = Inf)
    ######################################
    # So far I haven't tested this because I haven't had 
    # individual failures
    failures <- resps |>
      httr2::resps_failures() |>
      httr2::resps_requests()
    
    if(length(failures) > 0){
      message("There were", length(failures), "failed requests.")
    }
    
    return_list <- data.frame()
    for(resp in resps){
      df1 <- get_resp_data(resp)
      return_list <- rbind(return_list, df1)
    }
    
    ######################################
  } else {
    resps <- httr2::req_perform(req)
    return_list <- get_resp_data(resps)
  }

  return(return_list)
}


#' Coordinate the request and retrieval of OGC calls
#' 
#' @param args arguments from individual functions
#' @param output_id Name of id column to return
#' @param service Endpoint name.
#' @param max_results
#' 
#' @noRd
#' @return data.frame with attributes
get_ogc_data <- function(args,
                         output_id, 
                         service){
  
  args[["service"]] <-  service
  max_results <- args[["max_results"]]
  args[["max_results"]] <- NULL
  args <- switch_arg_id(args, 
                        id_name = output_id, 
                        service = service)
  
  properties <- args[["properties"]]
  args[["properties"]] <- switch_properties_id(properties, 
                                               id_name = output_id, 
                                               service = service)
  convertType <- args[["convertType"]] 
  args[["convertType"]] <- NULL

  req <- do.call(construct_api_requests, args)
  
  return_list <- walk_pages(req, max_results)
  
  if(is.na(args[["skipGeometry"]])){
    skipGeometry <- FALSE
  } else {
    skipGeometry <- args[["skipGeometry"]]
  }
  
  return_list <- deal_with_empty(return_list, properties, service,
                                 skipGeometry, convertType)
  
  if(convertType) return_list <- cleanup_cols(return_list, service = service)
  
  return_list <- rejigger_cols(return_list, properties, output_id)
  
  attr(return_list, "request") <- req
  attr(return_list, "queryTime") <- Sys.time()
  return(return_list)
}

