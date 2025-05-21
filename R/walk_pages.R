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
  
  returned_contents <- httr2::req_perform(req)
  
  if(httr2::resp_status(returned_contents) != 200){
    if(httr2::resp_status(returned_contents) == 429){
      stop("You hit your hourly limit for requests. To increase the limit, 
           see: https://api.waterdata.usgs.gov/docs/ogcapi/keys/")
    } 
  }
  
  header_info <- httr2::resp_headers(returned_contents)
  if(Sys.getenv("API_USGS_PAT") != ""){
    message("Remaining requests this hour:", header_info$`x-ratelimit-remaining`)
  }
  
  contents[[page]] <- returned_contents |> 
    httr2::resp_body_string() 
  
  json_content <- jsonlite::fromJSON(contents[[page]] )
  
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
    
    Tailcall(
      walk_pages_recursive,
      make_request,
      page + 1,
      contents
    )
  }
}