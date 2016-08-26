#' import data from the National Groundwater Monitoring Network \url{http://cida.usgs.gov/ngwmn/}.
#' 
#' Only water level data is currently available through the web service.  
#' @param asDateTime logical if \code{TRUE}, will convert times to POSIXct format.  Currently defaults to 
#' \code{FALSE} since time zone information is not included.  
#' @param featureID character Vector of feature IDs in the formatted with agency code and site number 
#' separated by a period, e.g. \code{USGS.404159100494601}.
#' @param service character Identifies which web service to access.  Only \code{observation} is currently 
#' supported, which retrieves all water level for each site.   
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided time zone offset).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @import utils
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' 
#' @examples 
#' \dontrun{
#' #one site
#' site <- "USGS.430427089284901"
#' oneSite <- readNGWMNdata(featureID = site)
#' 
#' #multiple sites
#' sites <- c("USGS.272838082142201","USGS.404159100494601", "USGS.401216080362703")
#' multiSiteData <- readNGWMNdata(sites)
#' 
#' #non-USGS site
#' site <- "MBMG.892195"
#' data <- readNGWMNdata(featureID = site)
#' 
#' #site with no data returns empty data frame
#' noDataSite <- "UTGS.401544112060301"
#' noDataSite <- readNGWMNdata(featureID = noDataSite, service = "observation")
#' }
#' 

readNGWMNdata <- function(featureID, service = "observation", asDateTime = TRUE, tz = ""){
  message("          ********************************************************
          DISCLAIMER: NGWMN retrieval functions are still in flux, 
              and no future behavior or output is guaranteed
          *********************************************************")
  match.arg(service, c("observation", "featureOfInterest"))
  
  
  if(service == "observation"){
    allObs <- NULL
    allAttrs <- NULL
    allSites <- NULL
    #these attributes are pulled out and saved when doing binds to be reattached
    attrs <- c("url","gml:identifier","generationDate")
    for(f in featureID){
      obsFID <- retrieveObservation(f, asDateTime, attrs)
      siteFID <- retrieveFeatureOfInterest(f, asDateTime)
      if(is.null(allObs)){
        allObs <- obsFID
        allSites <- siteFID
        allAttrs <- saveAttrs(attrs, allObs)
      }else{
        obsFIDatt <- saveAttrs(attrs, obsFID)
        obsFID <- removeAttrs(attrs, obsFID)
        allAttrs <- bind_rows(allAttrs, obsFIDatt)
        allObs <- bind_rows(allObs, obsFID)
        allSites <- bind_rows(allSites, siteFID)
      }
      attributes(allObs) <- c(attributes(allObs),as.list(allAttrs))
      attr(allObs, "siteInfo") <- allSites
      returnData <- allObs
    }
  }else if(service == "featureOfInterest"){
    allSites <- NULL
    for(f in featureID){
      siteFID <- retrieveFeatureOfInterest(f, asDateTime)
      if(is.null(allSites)){
        allSites <- siteFID
        }else{
        allSites <- bind_rows(allSites, siteFID)
      }
    }
    returnData <- allSites
  }else{
    stop("unrecognized service request")
  }
  
  return(returnData)
}

#' Retrieve groundwater levels from the National Ground Water Monitoring Network \url{http://cida.usgs.gov/ngwmn/}.
#'
#' @param featureID character Vector of feature IDs in the formatted with agency code and site number 
#' separated by a period, e.g. \code{USGS.404159100494601}.
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' #one site
#' site <- "USGS.430427089284901"
#' oneSite <- readNGWMNlevels(featureID = site)
#' 
#' #multiple sites
#' sites <- c("USGS.272838082142201","USGS.404159100494601", "USGS.401216080362703")
#' multiSiteData <- readNGWMNlevels(sites)
#' 
#' #non-USGS site
#' site <- "MBMG.892195"
#' data <- readNGWMNlevels(featureID = site)
#' 
#' #site with no data returns empty data frame
#' noDataSite <- "UTGS.401544112060301"
#' noDataSite <- readNGWMNlevels(featureID = noDataSite)
#' }

readNGWMNlevels <- function(featureID){
  data <- readNGWMNdata(featureID, service = "observation")
  return(data)
}

#' Retrieve site data from the National Ground Water Monitoring Network \url{http://cida.usgs.gov/ngwmn/}.
#'
#' @param featureID character Vector of feature IDs in the formatted with agency code and site number 
#' separated by a period, e.g. \code{USGS.404159100494601}.
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
#' \dontrun{
#' #one site
#' site <- "USGS.430427089284901"
#' oneSite <- readNGWMNsites(featureID = site)
#' 
#' #multiple sites
#' sites <- c("USGS.272838082142201","USGS.404159100494601", "USGS.401216080362703")
#' multiSiteInfo <- readNGWMNsites(sites)
#' 
#' #non-USGS site
#' site <- "MBMG.892195"
#' siteInfo <- readNGWMNsites(featureID = site)
#' 
#' }

readNGWMNsites <- function(featureID){
  sites <- readNGWMNdata(featureID, service = "featureOfInterest")
  return(sites)
}



retrieveObservation <- function(featureID, asDateTime, attrs){
  #will need to contruct this more piece by piece if other versions, properties are added
  baseURL <- "http://cida.usgs.gov/ngwmn_cache/sos?request=GetObservation&service=SOS&version=2.0.0&observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel&responseFormat=text/xml&featureOfInterest=VW_GWDP_GEOSERVER."
  url <- paste0(baseURL, featureID)
  
  returnData <- importNGWMN_wml2(url, asDateTime)
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
#don't expose until can support bbox
#note: import function can only do single sites right now
retrieveFeatureOfInterest <- function(featureID, asDateTime){
  baseURL <- "http://cida.usgs.gov/ngwmn_cache/sos?request=GetFeatureOfInterest&service=SOS&version=2.0.0&observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel&responseFormat=text/xml&featureOfInterest=VW_GWDP_GEOSERVER."
  url <- paste0(baseURL, featureID)
  siteDF <- importNGWMN_wml2(url, asDateTime)
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