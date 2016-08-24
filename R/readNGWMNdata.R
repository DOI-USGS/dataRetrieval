#' import data from the National Groundwater Monitoring Network \link{http://cida.usgs.gov/ngwmn/}.
#' 
#' Only water level data is currently available through the web service.  
#' @param asDateTime logical if \code{TRUE}, will convert times to POSIXct format.  Currently defaults to 
#' \code{FALSE} since time zone information is not included.  
#' @param featureID character Vector of feature IDs in the formatted with agency code and site number 
#' separated by a period, e.g. \code{USGS.404159100494601}.
#' @param request character Identifies which web service to access.  Only \code{observation} is currently 
#' supported, which retrieves all water level for each site.   
#' @import utils
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @export
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
#' noDataSite <- readNGWMNdata(featureID = noDataSite, request = "observation")
#' }
#' 

readNGWMNdata <- function(featureID, request = "observation", asDateTime = FALSE){
  
  match.arg(request, c("observation", "featureOfInterest"))
  if(asDateTime){
    warning("Times zones will be incorrect.  This will be fixed in the future")
  }
  
  if(request == "observation"){
    allObs <- NULL
    allAttrs <- NULL
    allSites <- NULL
    #these attributes are pulled out and saved when doing binds to be reattached
    attrs <- c("url","gml:identifier","generationDate")
    for(f in featureID){
      obsFID <- retrieveObservation(f, asDateTime, attrs)
      siteFID <- retrieveFeatureOfInterest(f)
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
  } #TODO: add direct feature of interest request
  
  return(returnData)
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
    returnData <- returnData[,c(7,1:6)] #move siteNum to the left
  }
  attributes(returnData) <- c(attributes(returnData), as.list(attribs))
  
  return(returnData)
}

#retrieve feature of interest
#don't expose until can support bbox
#note: import function can only do single sites right now
retrieveFeatureOfInterest <- function(featureID){
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