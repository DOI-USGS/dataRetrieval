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
    # by default, the data is put in POSIXct and seems
    # to be pretty smart about the offset/tzone
  }
  
  if("value" %in% names(df)){
    df$value <- as.numeric(df$value)
  }
  
  if("contributing_drainage_area" %in% names(df)){
    df$contributing_drainage_area <- as.numeric(df$contributing_drainage_area)
  }
  
  if("drainage_area" %in% names(df)){
    df$drainage_area <- as.numeric(df$drainage_area)
  }  
  
  df
}
