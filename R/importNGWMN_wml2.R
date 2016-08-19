#' Function to return data from the National Ground Water Monitoring Network waterML2 format
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#' 
#' @param input character or raw, containing the url for the retrieval or a path to the data file, or raw XML.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return mergedDF a data frame source, time, value, uom, uomTitle, comment, gmlID
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attr
#' @importFrom lubridate parse_date_time
#' @examples
#' \dontrun{
#' url <- "http://cida.usgs.gov/ngwmn_cache/sos?request=GetObservation&service=SOS&version=2.0.0&observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel&responseFormat=text/xml&featureOfInterest=VW_GWDP_GEOSERVER.USGS.403836085374401"
#' data <- importNGWMN_wml2(url)
#' }
importNGMWN_wml2 <- function(input, asDateTime=FALSE, tz=""){
  #TODO: update documentation
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }else{tz = "UTC"}
  
  raw <- FALSE
  if(class(input) == "character" && file.exists(input)){
    returnedDoc <- read_xml(input)
  }else if(class(input) == 'raw'){
    returnedDoc <- read_xml(input)
    raw <- TRUE
  } else {
    returnedDoc <- xml_root(getWebServiceData(input, encoding='gzip'))
  }
  
  response <- xml_name(returnedDoc)
  if(response == "GetObservationResponse"){
    
    timeSeries <- xml_find_all(returnedDoc, "//wml2:MeasurementTimeseries") #each parameter/site combo
    
    if(0 == length(timeSeries)){
      df <- data.frame()
      if(!raw){
        attr(df, "url") <- input
      }
      return(df)
    }
    
    mergedDF <- NULL
    
    for(t in timeSeries){
      gmlID <- xml_attr(t,"id")
      TVP <- xml_find_all(t, ".//wml2:MeasurementTVP")#time-value pairs
      time <- xml_text(xml_find_all(TVP,".//wml2:time"))
      if(asDateTime){
        time <- parse_date_time(time, c("%Y","%Y-%m-%d","%Y-%m-%dT%H:%M","%Y-%m-%dT%H:%M:%S",
                                        "%Y-%m-%dT%H:%M:%OS","%Y-%m-%dT%H:%M:%OS%z"), exact = TRUE)
        #^^setting tz in as.POSIXct just sets the attribute, does not convert the time!
        attr(time, 'tzone') <- tz 
      }
      valueNodes <- xml_find_all(TVP,".//wml2:value")
      values <- as.numeric(xml_text(valueNodes))
      nVals <- length(values)
      gmlID <- rep(gmlID, nVals)
      
      uom <- removeBlanks(xml_attr(valueNodes, "uom", default = NA))
      source <- xml_attr(xml_find_all(TVP, ".//wml2:source"), "title")
      comment <- removeBlanks(xml_text(xml_find_all(TVP, ".//wml2:comment")))
      
      df <- cbind.data.frame(source, time, value=values, uom, comment, gmlID,
                             stringsAsFactors=FALSE)
      if (is.null(mergedDF)){
        mergedDF <- df
      } else {
        similarNames <- intersect(colnames(mergedDF), colnames(df))
        mergedDF <- full_join(mergedDF, df, by=similarNames)
      }
    }
    
    if(!raw){
      url <- input
      attr(mergedDF, "url") <- url
    }
    attr(mergedDF, "gml:identifier") <- xml_text(xml_find_all(returnedDoc, ".//gml:identifier")) 
    attr(mergedDF, "generationDate") <- xml_text(xml_find_all(returnedDoc, ".//wml2:generationDate")) 
  }else if(response == "ExceptionReport"){
    #TODO: what happens if exception?
  }
  return(mergedDF)
  
}


#replace blank cells with NAs
removeBlanks <- function(input){
  input <- sapply(input, function(f){is.na(f)<-which(f == '');f})
  return(input)
}


