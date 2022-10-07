#' @name whatWQPsamples
#' @rdname wqpSpecials
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @export
#' @examples
#' \donttest{
#' 
#' site1 <- whatWQPsamples(siteid="USGS-01594440")
#' 
#' type <- "Stream"
#' sites <- whatWQPsamples(countycode="US:55:025",siteType=type)
#' 
#' lakeSites_samples <- whatWQPsamples(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
#' lakeSites_samples_chars <- whatWQPsamples(siteType = "Lake, Reservoir, Impoundment",
#'                               statecode = "US:55", convertType=FALSE)
#' 
#' }
whatWQPsamples <- function(..., convertType=TRUE){
  
  values <- readWQPdots(...)
  
  values <- values$values
  
  if("tz" %in% names(values)){
    values <- values[!(names(values) %in% "tz")]
  }
  
  if("service" %in% names(values)){
    values <- values[!(names(values) %in% "service")]
  }
  
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  
  baseURL <- drURL("Activity", arg.list = values)

  baseURL <- appendDrURL(baseURL, mimeType = "tsv")
  
  withCallingHandlers({
    retval <- importWQP(baseURL, zip=values["zip"] == "yes", convertType=convertType)
  }, warning=function(w) {
    if (any( grepl( "Number of rows returned not matched in header", w)))
      invokeRestart("muffleWarning")
  })

  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- baseURL
  
  return(retval)
}

#' @name whatWQPmetrics
#' @rdname wqpSpecials
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @export
#' @examples
#' \donttest{
#' 
#' type <- "Stream"
#' sites <- whatWQPmetrics(countycode="US:55:025",siteType=type)
#' lakeSites_metrics <- whatWQPmetrics(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
#' lakeSites_metrics_chars <- whatWQPmetrics(siteType = "Lake, Reservoir, Impoundment", 
#'                        statecode = "US:55", convertType=FALSE)
#' }
whatWQPmetrics <- function(..., convertType=TRUE){

  values <- readWQPdots(...)

  values <- values$values
  
  if("tz" %in% names(values)){
    values <- values[!(names(values) %in% "tz")]
  }
  
  if("service" %in% names(values)){
    values <- values[!(names(values) %in% "service")]
  }
  
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  
  baseURL <- drURL("ActivityMetric", arg.list = values)
  
  baseURL <- appendDrURL(baseURL, mimeType = "tsv")
  
  withCallingHandlers({
    retval <- importWQP(baseURL, zip=values["zip"] == "yes", convertType=convertType)
  }, warning=function(w) {
    if (any( grepl( "Number of rows returned not matched in header", w)))
      invokeRestart("muffleWarning")
  })

  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- baseURL

  return(retval)
}


#' Data Available from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on \url{https://www.waterqualitydata.us/webservices_documentation}.
#' The information returned from this function describes the
#' available data at the WQP sites, and some metadata on the sites themselves.
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options. A list of arguments can also be supplied.
#' @param saveFile path to save the incoming geojson output. 
#' @param convertType logical, defaults to \code{TRUE}. If \code{TRUE}, the function will convert the data to dates, datetimes,
#' numerics based on a standard algorithm. If false, everything is returned as a character.
#' @keywords data import WQP web service
#' @return A data frame with at least the following columns:
#' \tabular{lll}{ 
#' Name \tab Type \tab Description \cr
#' "type_a" \tab character \tab Geojson type \cr 
#' "features.type"  \tab character \tab Geojson feature type  \cr                     
#' "type1"   \tab character \tab Geojson spatial type \cr                            
#' "coordinates" \tab list \tab List of longitude/latitude \cr       
#' "ProviderName"  \tab character \tab 	The name of the database that provided the data to the Water Qaulity 
#' portal (E.G. STORET, NWIS, STEWARDS) \cr                     
#' "OrganizationIdentifier" \tab character \tab A designator used to 
#' uniquely identify a unique business establishment within a context. \cr             
#' "OrganizationFormalName"  \tab character \tab The legal designator (i.e. formal name) of an organization. \cr        
#' "MonitoringLocationIdentifier"  \tab character \tab 	A designator used to 
#' describe the unique name, number, or code assigned to identify the monitoring location. \cr 
#' "MonitoringLocationName"  \tab character \tab 	The designator specified by the sampling organization 
#' for the site at which sampling or other activities are conducted. \cr 
#' "MonitoringLocationTypeName" \tab character \tab 	The descriptive name for a type of monitoring location. \cr 
#' "ResolvedMonitoringLocationTypeName" \tab character \tab  \cr 
#' "HUCEightDigitCode"  \tab character \tab The 8 digit federal code used to identify the 
#' hydrologic unit of the monitoring location to the cataloging unit level of precision. \cr                 
#' "siteUrl"   \tab character \tab URL to site information \cr                          
#' "activityCount" \tab numeric \tab  \cr                    
#' "resultCount"  \tab numeric \tab  \cr                       
#' "StateName" \tab character \tab  State name \cr                        
#' "CountyName" \tab character \tab County name \cr  
#' }
#' 
#' @export
#' @seealso whatNWISsites
#' @examplesIf is_dataRetrieval_user()
#' \donttest{
#' site1 <- whatWQPdata(siteid="USGS-01594440")
#' 
#' type <- "Stream"
#' sites <- whatWQPdata(countycode="US:55:025",siteType=type)
#' 
#' lakeSites <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
#' lakeSites_chars <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment", 
#'                        statecode = "US:55", convertType=FALSE)
#' }
whatWQPdata <- function(..., saveFile = tempfile(), convertType=TRUE){

  values <- readWQPdots(...)
  
  values <- values$values
  
  if("tz" %in% names(values)){
    values <- values[!(names(values) %in% "tz")]
  }
  
  if("service" %in% names(values)){
    values <- values[!(names(values) %in% "service")]
  }
  
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  
  baseURL <- drURL("Station", arg.list = values)
  
  baseURL <- appendDrURL(baseURL, mimeType = "geojson")

  saveFile_zip <- saveFile
  if(tools::file_ext(saveFile) != ".zip"){
    saveFile_zip <- paste0(saveFile,".zip")
  }

  doc <- getWebServiceData(baseURL, httr::write_disk(saveFile_zip))
  if(is.null(doc)){
    return(invisible(NULL))
  }
  headerInfo <- attr(doc, "headerInfo")
  
  if(headerInfo$`total-site-count` == 0){
    y <- data.frame(total_type = character(),
                     lat = numeric(),
                     lon = numeric(),
                     ProviderName = character(),
                     OrganizationIdentifier = character(),
                     OrganizationFormalName = character(),
                     MonitoringLocationIdentifier = character(),
                     MonitoringLocationName = character(),
                     MonitoringLocationTypeName = character(),
                     ResolvedMonitoringLocationTypeName = character(),
                     HUCEightDigitCode = character(),
                     siteUrl = character(),
                     activityCount = numeric(),
                     resultCount = numeric(),
                     StateName = character(),
                     CountyName = character(),
                     stringsAsFactors=FALSE)
    
  } else {
    
    doc <- utils::unzip(saveFile_zip, exdir = saveFile)
    unlink(saveFile_zip)
  
    retval <- as.data.frame(jsonlite::fromJSON(doc), stringsAsFactors = FALSE)
    df_cols <- as.integer(which(sapply(retval, class) == "data.frame"))
    y <- retval[,-df_cols]
    
    for(i in df_cols){
      y <- cbind(y, retval[[i]])
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
  }
  
  attr(y, "queryTime") <- Sys.time()
  attr(y, "url") <- baseURL
  attr(y, "file") <- saveFile
  return(y)
}
