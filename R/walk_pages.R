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
  df
}

walk_pages <- function(req) {
  
  walk_pages_recursive(
    req = req,
    page = 1,
    contents = list()
  )
}

walk_pages_recursive <- function(req, page, contents) {
  
  url_method <- "GET"
  if(!is.null(req$body)){
    url_method <- "POST"
    body <- req$body
  }
  
  use_sf <- !grepl("skipGeometry=true", req$url, ignore.case = TRUE)
  
  message(url_method, ": ", req$url) 
  
  returned_contents <- req |> 
    httr2::req_perform()

  header_info <- httr2::resp_headers(returned_contents)
  if(Sys.getenv("API_USGS_PAT") != ""){
    message("Remaining requests this hour:", header_info$`x-ratelimit-remaining`)
  }
  
  contents[[page]] <- returned_contents |> 
    httr2::resp_body_string() 
  
  json_content <- jsonlite::fromJSON(contents[[page]] )
  
  if(isTRUE(json_content[["numberReturned"]] == 0)){
    return(data.frame())
  }
  
  next_page <- json_content$links[, "rel", drop = TRUE] == "next"
  
  if (!any(next_page)) {
    
    if(use_sf){
      return_df <- lapply(contents, function(x) {
        df_i <- sf::read_sf(x) 
      }) |> 
        do.call(what = rbind)
    } else {
      return_df <- lapply(contents, function(x) {
        df_i <- jsonlite::fromJSON(x)[["features"]][["properties"]] 
      }) |> 
        do.call(what = rbind)
    }
    
    return(return_df)
    
  } else {
    
    make_request <- httr2::req_url(req = req, 
                                   url = json_content$links[, "href", drop = TRUE][next_page])
    page <- page + 1
    while (TRUE){
      return(walk_pages_recursive(req = make_request,
                                  page = page,
                                  contents = contents))
    }

  }
}