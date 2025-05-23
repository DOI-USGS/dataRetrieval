deal_with_empty <- function(return_list, properties, service){
  if(nrow(return_list) == 0){
    if(is.na(properties)){
      schema <- check_OGC_requests(endpoint = service, type = "schema")
      properties <- names(schema$properties)
    }
    return_list <- data.frame(matrix(nrow = 0, ncol = length(properties)))
    names(return_list) <- properties
  }
  
  return(return_list)
}

rejigger_cols <- function(df, properties, service){
  new_id <- paste0(gsub("-", "_", service), "_id")
  names(df)[names(df) == "id"] <- new_id
  
  if(!all(is.na(properties))){
    properties[properties == "id"] <- new_id
    df <- df[, properties]
  }
  df
}

cleanup_cols <- function(df){
  
  if("qualifier" %in% names(df)){
    if(!all(is.na(df$qualifier))){
      df$qualifier <- vapply(X = df$qualifier,
                             FUN = function(x) paste(x, collapse = ", "),
                             FUN.VALUE =  c(NA_character_)) 
    }
  }
  
  if("time" %in% names(df)){
    df$time <- as.Date(df$time)
  }
  
  if("value" %in% names(df)){
    df$value <- as.numeric(df$value)
  }
  
  if("contributing_drainage_area" %in% names(df)){
    df$contributing_drainage_area <- as.numeric(df$contributing_drainage_area)
  }  
  
  df
}

next_req_url <- function(resp, req) {

  body <- httr2::resp_body_json(resp)
  
  if(isTRUE(body[["numberReturned"]] == 0)){
    return(NULL)
  }
  
  header_info <- httr2::resp_headers(resp)
  if(Sys.getenv("API_USGS_PAT") != ""){
    message("Remaining requests this hour:", header_info$`x-ratelimit-remaining`)
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

walk_pages <- function(req){
  
  resps <- httr2::req_perform_iterative(req, next_req = next_req_url) 

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

  attr(return_list, "request") <- req
  attr(return_list, "queryTime") <- Sys.time()
  
  return(return_list)
}
