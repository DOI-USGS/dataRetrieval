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
#' \dontrun{
#' site1 <- whatWQPsamples(siteid="USGS-01594440")
#' 
#' type <- "Stream"
#' sites <- whatWQPsamples(countycode="US:55:025",siteType=type)
#' lakeSites <- whatWQPsamples(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
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
#' \dontrun{
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
#' @keywords data import WQP web service
#' @importFrom jsonlite fromJSON
#' @return A list
#' 
#' @export
#' @import utils
#' @examples
#' \dontrun{
#' site1 <- whatWQPdata(siteid="USGS-01594440")
#' 
#' type <- "Stream"
#' sites <- whatWQPdata(countycode="US:55:025",siteType=type)
#' 
#' library(leaflet)
#' library(geojsonio)
#' leaflet() %>% 
#'      addGeoJSON(geojson_read(attr(sites,"file"))) %>% 
#'      addProviderTiles("CartoDB.Positron")
#' 
#' lakeSites <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
#' }
whatWQPdata <- function(...){

  message("DISCLAIMER: This function is still in flux, 
              and no future behavior or output is guaranteed")
  
  values <- readWQPdots(...)
  
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- drURL("wqpStation")
  urlCall <- paste0(baseURL,
                    urlCall,
                    "&mimeType=geojson&sorted=no")
  temp <- tempfile()
  temp <- paste0(temp,".geojson")
  doc <- getWebServiceData(urlCall, write_disk(temp))
  headerInfo <- attr(doc, "headerInfo")

  retval <- fromJSON(temp)
  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- urlCall
  attr(retval, "file") <- temp
  return(retval)
}
