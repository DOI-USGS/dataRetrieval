#' Function to return data from the WaterML2 data
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#'
#' @param obs_url character or raw, containing the url for the retrieval or a path to the data file, or raw XML.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
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
#' @importFrom lubridate parse_date_time
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
importWaterML2 <- function(obs_url, asDateTime=FALSE, tz=""){
  
  if(tz != ""){
    tz <- match.arg(tz, c("America/New_York","America/Chicago",
                          "America/Denver","America/Los_Angeles",
                          "America/Anchorage","America/Honolulu",
                          "America/Jamaica","America/Managua",
                          "America/Phoenix","America/Metlakatla"))
  }else{tz = "UTC"}
  
  raw <- FALSE
  if(class(obs_url) == "character" && file.exists(obs_url)){
    returnedDoc <- read_xml(obs_url)
  }else if(class(obs_url) == 'raw'){
    returnedDoc <- read_xml(obs_url)
    raw <- TRUE
  } else {
    returnedDoc <- xml_root(getWebServiceData(obs_url, encoding='gzip'))
  }
  
  timeSeries <- xml_find_all(returnedDoc, "//wml2:Collection") #each parameter/site combo
  
  if(0 == length(timeSeries)){
    df <- data.frame()
    if(!raw){
      attr(df, "url") <- obs_url
    }
    return(df)
  }
  
  mergedDF <- NULL
  
  for(t in timeSeries){
    TVP <- xml_find_all(t, ".//wml2:MeasurementTVP")#time-value pairs
    time <- xml_text(xml_find_all(TVP,".//wml2:time"))
    #TODO: if asDateTime....
    if(asDateTime){
      time <- parse_date_time(time, c("%Y","%Y-%m-%d","%Y-%m-%dT%H:%M","%Y-%m-%dT%H:%M:%S",
                                      "%Y-%m-%dT%H:%M:%OS","%Y-%m-%dT%H:%M:%OS%z"), exact = TRUE)
      #^^setting tz in as.POSIXct just sets the attribute, does not convert the time!
      attr(time, 'tzone') <- tz 
    }
    values <- as.numeric(xml_text(xml_find_all(TVP,".//wml2:value")))
    #TODO: deal with multiple identifiers (assigning column names)
    idents <- xml_text(xml_find_all(t, ".//gml:identifier"))
    useIdents <- rep(idents, length(values))
    #TODO: check qualifiers in points against default, if both exist, same, etc
    tvpQuals <- xml_text(xml_find_all(TVP, ".//swe:description"))
    defaultPointMeta <- xml_find_all(t, ".//wml2:DefaultTVPMeasurementMetadata")
    defaultQuals <- xml_text(xml_find_all(defaultPointMeta, ".//swe:description"))
    
    if(length(tvpQuals) == 0){
      useQuals <- rep(defaultQuals, length(values))
    }else{
      useQuals <- tvpQuals
    }
    if(length(useQuals) == 0){
      df <- cbind.data.frame(time, value=values, identifier=useIdents,
                             stringsAsFactors=FALSE)
    }else{
    df <- cbind.data.frame(time, value=values, qualifier=useQuals, identifier=useIdents,
                           stringsAsFactors=FALSE)
    }
    if (is.null(mergedDF)){
      mergedDF <- df
    } else {
      similarNames <- intersect(colnames(mergedDF), colnames(df))
      mergedDF <- full_join(mergedDF, df, by=similarNames)
    }
  }
  return(mergedDF)
}


