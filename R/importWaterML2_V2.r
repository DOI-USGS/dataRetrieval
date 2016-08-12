#' Function to return data from the WaterML2 data
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#'
#' @param obs_url character containing the url for the retrieval or a file path to the data file.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, Date
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return mergedDF a data frame time, value, description, qualifier, and identifier
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attr
#' @importFrom dplyr rbind_all
#' @examples
#' baseURL <- "http://waterservices.usgs.gov/nwis/dv/?format=waterml,2.0"
#' URL <- paste(baseURL, "sites=01646500",
#'      "startDT=2014-09-01",
#'      "endDT=2014-09-08",
#'      "statCd=00003",
#'      "parameterCd=00060",sep="&")
#' URL2 <- paste("http://cida.usgs.gov/noreast-sos/simple?request=GetObservation",
#'      "featureID=MD-BC-BC-05",
#'      "offering=RAW",
#'      "observedProperty=WATER",sep="&")
#' \dontrun{
#' dataReturned1 <- importWaterML2(URL)
#' dataReturn2 <- importWaterML2(URL2, TRUE)
#' URLmulti <-  paste(baseURL,
#'   "sites=04024430,04024000",
#'   "startDT=2014-09-01",
#'   "endDT=2014-09-08",
#'   "statCd=00003",
#'   "parameterCd=00060",sep="&")
#' dataReturnMulti <- importWaterML2(URLmulti)
#' }
#' filePath <- system.file("extdata", package="dataRetrieval")
#' fileName <- "WaterML2Example.xml"
#' fullPath <- file.path(filePath, fileName)
#' UserData <- importWaterML2(fullPath)
#' 
importWaterML2_V2 <- function(input, asDateTime=FALSE, tz=""){
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }
  
  raw <- FALSE
  if(class(input) == "character" && file.exists(input)){
    returnedDoc <- read_xml(input)
  }else if(class(input) == 'raw'){
    returnedDoc <- read_xml(input)
    raw <- TRUE
  } else {
    returnedDoc <- xml_root(getWebServiceData(input, encoding='gzip'))
  }
  #if(xml_name(xml_root(returnedDoc)) == "FeatureCollection"){
    timeSeries <- xml_find_all(returnedDoc, "//wml2:Collection") #each parameter/site combo
  #}else{
   # timeSeries <- returnedDoc #sometimes there isn't the gml:FeatureCollection root
  #}
  
  if(0 == length(timeSeries)){
    df <- data.frame()
    if(!raw){
      attr(df, "url") <- obs_url
    }
    return(df)
  }
  
    for(t in timeSeries){
      TVP <- xml_find_all(t, ".//wml2:MeasurementTVP")#time-value pairs
      time <- xml_text(xml_find_all(TVP,".//wml2:time"))
      #TODO: if asDateTime....
      if(asDateTime){
        
        time <- gsub(":","",DF2$time)
        time <- ifelse(nchar(DF2$time) > 18,
                       as.POSIXct(DF2$time, format="%Y-%m-%dT%H%M%S%z",tz="UTC"),
                       as.POSIXct(DF2$time, format="%Y-%m-%dT%H%M%S",tz="UTC"))
        
        time <- as.POSIXct(DF2$time, origin = "1970-01-01", tz="UTC")
        
        if(tz != ""){
          attr(time, "tzone") <- tz
        }
      } else {
        time <- as.Date(time)
      }
    }
    
    values <- xml_text(xml_find_all(TVP,".//wml2:value"))
    #TODO: deal with multiple identifiers (assigning column names)
    idents <- xml_text(xml_find_all(t, ".//gml:identifier"))
    
    #TODO: check qualifiers in points against default, if both exist, same, etc
    tvpQuals <- xml_attr(xml_find_all(TVP, ".//wml2:qualifier"), "xlink:title")
    defaultQuals <- xml_attr(xml_find_all(defaultPointMeta, ".//wml2:qualifier"),"title")
    
    if()
  }
  
  
}
