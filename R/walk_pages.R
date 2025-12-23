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
                                          max_reqs = Inf, on_error = "return")
    failures <- resps |>
      httr2::resps_failures() |>
      httr2::resps_requests()
    
    if(length(failures) > 0){
      stop(resps[[1]][["message"]])
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
  
  if(isTRUE(body[["code"]] == "InvalidQuery")){
    message(body[["description"]])
    return(NULL)
  }
  
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


get_csv <- function(req, max_results){
  
  if(is.na(max_results)){
    max_results <- 50000
  }

  message("Requesting:\n", req$url)
  skip_geo <- grepl("skipGeometry=true", req$url, ignore.case = TRUE)
  resp <- httr2::req_perform(req)

  if(httr2::resp_has_body(resp)){
    return_list <- httr2::resp_body_string(resp) 
    df <- suppressMessages(readr::read_csv(file = return_list))
    if(skip_geo){
      df <- df[, names(df)[!names(df) %in% c("x", "y")]]
    } else {
      df <- sf::st_as_sf(df, coords = c("x","y"))
      sf::st_crs(df) <- 4269
    }
    
    if(nrow(df) == max_results){
      warning("Missing data is probable. Use no_paging = FALSE to 
ensure all requested data is returned.")
    }    
  } else {
    df <- data.frame()
  }
  
  return(df)
}

