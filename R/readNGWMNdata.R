#' import data from the National Groundwater Monitoring Network \link{http://cida.usgs.gov/ngwmn/}
#' Only water level data and site metadata is currently available through the web service.  
#' @param asDateTime logical 
#' @param featureID character
#' @param request character
#' @import utils
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @export
#' 
#TODO: documentation once more solidified
readNGWMNdata <- function(featureID, request = "observation", asDateTime = TRUE){
  
  match.arg(request, c("observation", "featureOfInterest"))
  
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
      }else{
        #TODO: can attaching attributes wait till the end?
        obsFIDatt <- saveAttrs(attrs, obsFID)
        allAttrs <- saveAttrs(attrs, allObs)
        allObs <- removeAttrs(attrs, allObs)
        obsFID <- removeAttrs(attrs, obsFID)
        allAttrs <- bind_rows(allAttrs, obsFIDatt)
        allObs <- bind_rows(allObs, obsFID)
        attributes(allObs) <- c(attributes(allObs),as.list(allAttrs))
        allSites <- bind_rows(allSites, siteFID)
      }
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
  
  returnData <- importNGMWN_wml2(url, asDateTime)
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
  siteDF <- importNGMWN_wml2(url, asDateTime)
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