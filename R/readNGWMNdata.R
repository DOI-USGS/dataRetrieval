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
    for(f in featureID){
      obsFID <- retrieveObservation(f, asDateTime)
      if(is.null(allObs)){
        allObs <- obsFID
      }else{
        allObs <- bind_rows(allObs, obsFID) 
      }
    }
    returnData <- allObs
    
  } 
  
  return(returnData)
}

retrieveObservation <- function(featureID, asDateTime){
  #allow for multiple site calls, and aggregate here?
  #will need to contruct this more piece by piece if other versions, properties are added
  baseURL <- "http://cida.usgs.gov/ngwmn_cache/sos?request=GetObservation&service=SOS&version=2.0.0&observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel&responseFormat=text/xml&featureOfInterest=VW_GWDP_GEOSERVER."
  url <- paste0(baseURL, featureID)
  
  returnData <- importNGMWN_wml2(url, asDateTime)
  #tack on site number
  if(nrow(returnData) > 0){
    siteNum <- rep(sub('.*\\.', '', featureID), nrow(returnData))
    returnData <- mutate(returnData, site = siteNum)
    returnData <- returnData[,c(7,1:6)] #move siteNum to the left
  }
  return(returnData)
}
