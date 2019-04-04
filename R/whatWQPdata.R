#' Sampling Activity Data Import from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on \url{https://www.waterqualitydata.us/webservices_documentation}
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options. A list of arguments can also be supplied.
#' @keywords data import WQP web service
#' @return A data frame 
#' 
#' @export
#' @import utils
#' @examples
#' \donttest{
#' site1 <- whatWQPsamples(siteid="USGS-01594440")
#' 
#' type <- "Stream"
#' sites <- whatWQPsamples(countycode="US:55:025",siteType=type)
#' }
whatWQPsamples <- function(...){
  
  values <- readWQPdots(...)
  
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- drURL("wqpActivity")
  urlCall <- paste0(baseURL,
                    urlCall,
                    "&mimeType=tsv&sorted=no")
  
  retval <- importWQP(urlCall, zip=values["zip"] == "yes")
  
  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- urlCall
  
  return(retval)
}

#' Activity Metrics from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on \url{https://www.waterqualitydata.us/webservices_documentation}
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options. A list of arguments can also be supplied.
#' @keywords data import WQP web service
#' @return A data frame 
#' 
#' @export
#' @import utils
#' @examples
#' \donttest{
#' 
#' type <- "Stream"
#' sites <- whatWQPmetrics(countycode="US:55:025",siteType=type)
#' lakeSites <- whatWQPmetrics(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
#' }
whatWQPmetrics <- function(...){

  values <- readWQPdots(...)

  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))

  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")


  baseURL <- drURL("wqpMetrics")
  urlCall <- paste0(baseURL,
                    urlCall,
                    "&mimeType=tsv&sorted=no")

  retval <- importWQP(urlCall, zip=values["zip"] == "yes")

  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- urlCall

  return(retval)
}



#' Data Available from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on \url{https://www.waterqualitydata.us/webservices_documentation}
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options. A list of arguments can also be supplied.
#' @param saveFile path to save the incoming geojson output. 
#' @keywords data import WQP web service
#' @return A list
#' 
#' @export
#' @import utils
#' @examples
#' \donttest{
#' site1 <- whatWQPdata(siteid="USGS-01594440")
#' 
#' type <- "Stream"
#' sites <- whatWQPdata(countycode="US:55:025",siteType=type)
#' 
#' lakeSites <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
#' }
whatWQPdata <- function(..., saveFile = tempfile()){

  values <- readWQPdots(...)
  
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- drURL("wqpStation")
  urlCall <- paste0(baseURL,
                    urlCall,
                    "&mimeType=geojson&sorted=no")
  
  if(tools::file_ext(saveFile) != ".geojson"){
    saveFile <- paste0(saveFile,".geojson")
  }
  
  doc <- getWebServiceData(urlCall, httr::write_disk(saveFile))
  headerInfo <- attr(doc, "headerInfo")

  retval <- as.data.frame(jsonlite::fromJSON(saveFile), stringsAsFactors = FALSE)
  df_cols <- as.integer(which(sapply(retval, class) == "data.frame"))
  y <- retval[,-df_cols]
  
  for(i in df_cols){
    y <- dplyr::bind_cols(y, retval[[i]])
  }
  
  y[,grep("Count$",names(y))] <- sapply(y[,grep("Count$",names(y))], as.numeric)
  
  names(y)[names(y) == "type"] <- paste("type",letters[1:length(names(y)[names(y) == "type"])],sep="_")
  
  if(all(c("type_a","type_b","features.type") %in% names(y))){
    y$total_type <- paste(y$type_a, y$features.type, y$type_b)
    y$type_a <- NULL
    if(all(y$type_b == "Point")){
      y$lon <- sapply(y$coordinates, function(x) x[[1]][1])
      y$lat <- sapply(y$coordinates, function(x) x[[2]][1])
    }
    y$type_b <- NULL
    y$coordinates <- NULL
    y$features.type <- NULL
    y <- y[,c("total_type","lat","lon",names(y)[!(names(y) %in% c("total_type","lat","lon"))])]
  }

  
  attr(y, "queryTime") <- Sys.time()
  attr(y, "url") <- urlCall
  attr(y, "file") <- saveFile
  return(y)
}
