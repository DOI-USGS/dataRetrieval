#' Return a data frame if there's an empty response
#' 
#' @param return_list data frame returned from walk_pages
#' @param properties A vector of requested columns
#' @param service character, can be any existing collection such
#' as "daily", "monitoring-locations", "time-series-metadata"
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
deal_with_empty <- function(return_list, properties, service){
  if(nrow(return_list) == 0){
    if(all(is.na(properties))){
      schema <- check_OGC_requests(endpoint = service, type = "schema")
      properties <- names(schema$properties)
    }
    return_list <- data.frame(matrix(nrow = 0, ncol = length(properties)))
    names(return_list) <- properties
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
#'                                       properties = c("time", "value", "id"),
#'                                       service = "daily")  
#' df2 <- dataRetrieval:::rejigger_cols(df, 
#'                                      properties = c("value", "id", "time"),
#'                                      service = "daily")
#' 
rejigger_cols <- function(df, properties, service){
  new_id <- paste0(gsub("-", "_", service), "_id")
  names(df)[names(df) == "id"] <- new_id
  
  if(!all(is.na(properties))){
    properties[properties == "id"] <- new_id
    df <- df[, properties]
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
    
    ################################################
    # This offset check will be going away 
    # offset should be replaced by "cursor" eventually.
    offset <- as.integer(sub("(?i).*?\\boffset=?\\s*(\\d+).*", "\\1", next_url))
    if(isTRUE(offset > 40000)){
      warning("Not all data was returned! Split up the query for best results.")
      return(NULL)
    }
    ################################################
    
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
  
  if(use_sf){
    return_df <- sf::read_sf(httr2::resp_body_string(resp))
  } else {
    return_df <- jsonlite::fromJSON(httr2::resp_body_string(resp))[["features"]][["properties"]] 
  }

  return(return_df)
  
}

#' Walk through the pages
#' 
#' @param req httr2 initial request
#' 
#' @noRd
#' @return data.frame with attributes
walk_pages <- function(req){
  
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
  ######################################
  
  return_list <- data.frame()
  for(resp in resps){
    df1 <- get_resp_data(resp)
    return_list <- rbind(return_list, df1)
  }

  attr(return_list, "request") <- req
  attr(return_list, "queryTime") <- Sys.time()
  
  return(return_list)
}
