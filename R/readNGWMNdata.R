#' import data from the National Groundwater Monitoring Network \url{http://cida.usgs.gov/ngwmn/}.
#' 
#' Only water level data and site locations and names are currently available through the web service.  
#' @param service char Service for the request - "observation" and "featureOfInterest" are implemented.
#' @param \dots Other parameters to supply, namely \code{siteNumbers} or \code{bbox}
#' @param asDateTime logical if \code{TRUE}, will convert times to POSIXct format.  Currently defaults to 
#' \code{FALSE} since time zone information is not included.  
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the 
#' date times to UTC, properly accounting for daylight savings times based on the data's provided time zone offset.
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()} 
#' for more information on time zones.
#' @import utils
#' @importFrom dplyr mutate
#' @export
#' @examples 
#' \donttest{
#' #one site
#' site <- "USGS.430427089284901"
#' oneSite <- readNGWMNdata(siteNumbers = site, service = "observation")
#' 
#' #multiple sites
#' sites <- c("USGS.272838082142201","USGS.404159100494601", "USGS.401216080362703")
#' multiSiteData <- readNGWMNdata(siteNumbers = sites, service = "observation")
#' attributes(multiSiteData)
#' 
#' #non-USGS site
#' #accepts colon or period between agency and ID
#' site <- "MBMG:702934"
#' data <- readNGWMNdata(siteNumbers = site, service = "featureOfInterest")
#' 
#' #site with no data returns empty data frame
#' noDataSite <- "UTGS.401544112060301"
#' noDataSite <- readNGWMNdata(siteNumbers = noDataSite, service = "observation")
#' 
#' #bounding box
#' bboxSites <- readNGWMNdata(service = "featureOfInterest", bbox = c(30, -102, 31, 99))
#' #retrieve  sites.  Set asDateTime to false since one site has an invalid date
#' bboxData <- readNGWMNdata(service = "observation", siteNumbers = bboxSites$site[1:3], 
#' asDateTime = FALSE)
#' }
#' 
readNGWMNdata <- function(service, ..., asDateTime = TRUE, tz = "UTC"){
  message("DISCLAIMER: NGWMN retrieval functions are still in flux, 
              and no future behavior or output is guaranteed")
  
  dots <- convertLists(...)
  
  match.arg(service, c("observation", "featureOfInterest"))
  
  if(service == "observation"){
    allObs <- data.frame()
    allAttrs <- data.frame()
    
    #these attributes are pulled out and saved when doing binds to be reattached
    attrs <- c("url","gml:identifier","generationDate","responsibleParty", "contact")
    featureID <- stats::na.omit(gsub(":",".",dots[['siteNumbers']]))
    
    for(f in featureID){
      obsFID <- retrieveObservation(featureID = f, asDateTime, attrs, tz = tz)
      obsFIDattr <- saveAttrs(attrs, obsFID)
      obsFID <- removeAttrs(attrs, obsFID)
      allObs <- dplyr::bind_rows(allObs, obsFID)
      allAttrs <- dplyr::bind_rows(allAttrs, obsFIDattr)
      
    }
    
    allSites <- tryCatch({
      retrieveFeatureOfInterest(featureID = featureID)
    }, error = function(cond){return(NULL)})
    
    if(!is.null(allSites)){
      attr(allObs, "siteInfo") <- allSites
    }
    
    attr(allObs, "other") <- allAttrs
    returnData <- allObs
    
  } else if (service == "featureOfInterest") {
    
    if("siteNumbers" %in% names(dots)){
      featureID <- stats::na.omit(gsub(":",".",dots[['siteNumbers']]))
      allSites <- tryCatch({
        retrieveFeatureOfInterest(featureID = featureID)
      })
    }
    
    if("bbox" %in% names(dots)){
      allSites <- tryCatch({
        retrieveFeatureOfInterest(bbox=dots[['bbox']])
      })
    }
    returnData <- allSites
    
  } 
  
  return(returnData)
}

#' Retrieve groundwater levels from the National Ground Water Monitoring Network \url{http://cida.usgs.gov/ngwmn/}.
#'
#' @param siteNumbers character Vector of feature IDs formatted with agency code and site number 
#' separated by a period or semicolon, e.g. \code{USGS.404159100494601}.
#' @param asDateTime logical Should dates and times be converted to date/time objects,
#' or returned as character?  Defaults to \code{TRUE}.  Must be set to \code{FALSE} if a site 
#' contains non-standard dates.
#' @param tz character to set timezone attribute of dateTime. Default is "UTC", and converts the 
#' date times to UTC, properly accounting for daylight savings times based on the data's provided time zone offset.
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage", as well as the following which do not use daylight savings time: "America/Honolulu",
#' "America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla". See also  \code{OlsonNames()} 
#' for more information on time zones.
#' @export
#' 
#' @examples 
#' \donttest{
#' #one site
#' site <- "USGS.430427089284901"
#' oneSite <- readNGWMNlevels(siteNumbers = site)
#' 
#' #multiple sites
#' sites <- c("USGS:272838082142201","USGS:404159100494601", "USGS:401216080362703")
#' multiSiteData <- readNGWMNlevels(sites)
#' 
#' #non-USGS site
#' site <- "MBMG.892195"
#' data <- readNGWMNlevels(siteNumbers = site, asDateTime = FALSE)
#' 
#' #site with no data returns empty data frame
#' noDataSite <- "UTGS.401544112060301"
#' noDataSite <- readNGWMNlevels(siteNumbers = noDataSite)
#' }
readNGWMNlevels <- function(siteNumbers, asDateTime = TRUE, tz = "UTC"){
  data <- readNGWMNdata(siteNumbers = siteNumbers, service = "observation",
                        asDateTime = asDateTime, tz = tz)
  return(data)
}

#' Retrieve site data from the National Ground Water Monitoring Network \url{http://cida.usgs.gov/ngwmn/}.
#'
#' @param siteNumbers character Vector of feature IDs formatted with agency code and site number 
#' separated by a period or semicolon, e.g. \code{USGS.404159100494601}.
#' 
#' @export
#' @return A data frame the following columns: 
#' #' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' site \tab char \tab Site FID \cr
#' description \tab char \tab Site description \cr
#' dec_lat_va, dec_lon_va \tab numeric \tab Site latitude and longitude \cr
#' }
#' @examples 
#' \donttest{
#' #one site
#' site <- "USGS.430427089284901"
#' oneSite <- readNGWMNsites(siteNumbers = site)
#' 
#' #multiple sites
#' sites <- c("USGS:272838082142201","USGS:404159100494601", "USGS:401216080362703")
#' multiSiteInfo <- readNGWMNsites(sites)
#' 
#' #non-USGS site
#' site <- "MBMG.892195"
#' siteInfo <- readNGWMNsites(siteNumbers = site)
#' 
#' }
readNGWMNsites <- function(siteNumbers){
  sites <- readNGWMNdata(siteNumbers = siteNumbers, service = "featureOfInterest")
  return(sites)
}

retrieveObservation <- function(featureID, asDateTime, attrs, tz){
  url <- drURL(base.name = "NGWMN", access = pkg.env$access, request = "GetObservation", 
               service = "SOS", version = "2.0.0", observedProperty = "urn:ogc:def:property:OGC:GroundWaterLevel",
               responseFormat = "text/xml", featureOfInterest = paste("VW_GWDP_GEOSERVER", featureID, sep = "."))
  
  returnData <- importNGWMN(url, asDateTime=asDateTime, tz = tz)
  if(nrow(returnData) == 0){
    #need to add NA attributes, so they aren't messed up when stored as DFs
    attr(returnData, "gml:identifier") <- NA
    attr(returnData, "generationDate") <- NA
  }
  
  #mutate removes the attributes, need to save and append
  attribs <- saveAttrs(attrs, returnData)
  if(nrow(returnData) > 0){
    #tack on site number
    siteNum <- rep(sub('.*\\.', '', featureID), nrow(returnData))
    returnData <- mutate(returnData, site = siteNum)
    numCol <- ncol(returnData)
    returnData <- returnData[,c(numCol,1:(numCol - 1))] #move siteNum to the left
  }
  attributes(returnData) <- c(attributes(returnData), as.list(attribs))
  
  return(returnData)
}

#retrieve feature of interest
#could allow pass through srsName - needs to be worked in higher-up in dots
retrieveFeatureOfInterest <- function(..., asDateTime, srsName="urn:ogc:def:crs:EPSG::4269"){
  
  dots <- convertLists(...)
  
  values <- sapply(dots, function(x) as.character(paste(eval(x),collapse=",",sep="")))
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  # values <- gsub(x = convertDots(dots), pattern = ",", replacement = "%2C")
  
  url <- drURL(base.name = "NGWMN", access = pkg.env$access, request = "GetFeatureOfInterest", 
               service = "SOS", version = "2.0.0", responseFormat = "text/xml")
  
  if("featureID" %in% names(values)){
    url <- appendDrURL(url, featureOfInterest = paste("VW_GWDP_GEOSERVER", 
                                                      values[['featureID']], sep = "."))
    
  } else if("bbox" %in% names(values)) {
    url <- appendDrURL(url, bbox = paste(values[['bbox']], collapse=","),
                       srsName = srsName)
  } else {
    stop("Geographical filter not specified. Please use siteNumbers or bbox")
  }
  
  siteDF <- importNGWMN(url, asDateTime, tz = "")
  attr(siteDF, "url") <- url
  attr(siteDF, "queryTime") <- Sys.time()
  return(siteDF)
}


#save specified attributes from a data frame
saveAttrs <- function(attrs, df){
  attribs <- sapply(attrs, function(x) attr(df, x))
  if(is.vector(attribs)){
    toReturn <- as.data.frame(t(attribs), stringsAsFactors = FALSE)
  }else{ #don't need to transpose 
    toReturn <- as.data.frame(attribs, stringsAsFactors = FALSE)
  }
  return(toReturn)
}

#strip specified attributes from a data frame
removeAttrs <- function(attrs, df){
  for(a in attrs){
    attr(df, a) <- NULL
  }
  return(df)
}