#' Function to return data from the National Ground Water Monitoring Network waterML2 format
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#' 
#' @param input character or raw, containing the url for the retrieval or a path to the data file, or raw XML.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
#' @param tz character to set timezone attribute of datetime. Default is an empty quote, which converts the 
#' datetimes to UTC (properly accounting for daylight savings times based on the data's provided time zone offset).
#' Possible values to provide are "America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla"
#' @return mergedDF a data frame source, time, value, uom, uomTitle, comment, gmlID
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attr
#' @importFrom xml2 xml_find_first
#' @importFrom lubridate parse_date_time
#' @examples
#' \dontrun{
#' url <- "http://cida.usgs.gov/ngwmn_cache/sos?request=GetObservation&service=SOS&version=2.0.0
#' &observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel&responseFormat=text/xml&featureOf
#' Interest=VW_GWDP_GEOSERVER.USGS.403836085374401"
#' data <- importNGWMN_wml2(url)
#' 
#' url <- "http://cida.usgs.gov/ngwmn_cache/sos?request=GetObservation&service=SOS&version=2.0.0
#' &observedProperty=urn:ogc:def:property:OGC:GroundWaterLevel&responseFormat=text/xml&featureOf
#' Interest=VW_GWDP_GEOSERVER.USGS.474011117072901"
#' data <- importNGWMN_wml2(url)
#' }
#' 
#' 
#TODO: separate id and agency name, give also as separate dimensions
importNGWMN_wml2 <- function(input, asDateTime=FALSE, tz=""){
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
      rawTime <- xml_text(xml_find_all(TVP,".//wml2:time"))
      
      valueNodes <- xml_find_all(TVP,".//wml2:value")
      values <- as.numeric(xml_text(valueNodes))
      nVals <- length(values)
      gmlID <- rep(gmlID, nVals)
      
      #df of date, time, dateTime
      oneCol <- rep(NA, nVals) 
      timeDF <- data.frame(date=oneCol, time=oneCol, dateTime=oneCol)
      splitTime <- data.frame(matrix(unlist(strsplit(rawTime, "T")), nrow=nVals, byrow = TRUE), stringsAsFactors=FALSE)
      if(ncol(splitTime) > 1){ #some sites only have a date
        names(splitTime) <- c("date", "time")
      }else{
        names(splitTime) <- "date"
        splitTime <- mutate(splitTime, time = NA)
      }
      
      timeDF <- mutate(splitTime, dateTime = NA)
      logicVec <- nchar(rawTime) > 19
      timeDF$dateTime[logicVec] <- rawTime[logicVec]
      if(asDateTime){
        timeDF$dateTime <- parse_date_time(timeDF$dateTime, c("%Y","%Y-%m-%d","%Y-%m-%dT%H:%M","%Y-%m-%dT%H:%M:%S",
                                        "%Y-%m-%dT%H:%M:%OS","%Y-%m-%dT%H:%M:%OS%z"), exact = TRUE)
        #^^setting tz in as.POSIXct just sets the attribute, does not convert the time!
        attr(time, 'tzone') <- tz 
      }
       
      
      
      uom <- xml_attr(valueNodes, "uom", default = NA)
      source <- xml_attr(xml_find_all(TVP, ".//wml2:source"), "title")
      comment <- xml_text(xml_find_all(TVP, ".//wml2:comment"))
      
      df <- cbind.data.frame(source, timeDF, value=values, uom, comment, gmlID,
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
    mergedDF$date <- as.Date(mergedDF$date)
    nonDateCols <- grep("date",names(mergedDF), value=TRUE, invert = TRUE)
    
    mergedDF[nonDateCols][mergedDF[nonDateCols] == "" | mergedDF[nonDateCols]== -999999.0] <- NA
    attr(mergedDF, "gml:identifier") <- xml_text(xml_find_all(returnedDoc, ".//gml:identifier")) 
    attr(mergedDF, "generationDate") <- xml_text(xml_find_all(returnedDoc, ".//wml2:generationDate")) 
    meta <- xml_find_all(returnedDoc, ".//gmd:contact")
    attr(mergedDF, "contact") <- xml_attr(meta, "href")
    attr(mergedDF, "responsibleParty") <- xml_text(xml_find_all(meta, ".//gco:CharacterString"))
    
    
  }else if(response == "GetFeatureOfInterestResponse"){
    featureMembers <- xml_find_all(returnedDoc, ".//sos:featureMember")
    site <- xml_text(xml_find_all(featureMembers,".//gml:identifier"))
    site <- substring(site, 8)
    
    #some sites don't have a description
    siteDesc <- xml_text(xml_find_first(featureMembers, ".//gml:description"))
    
    siteLocs <- strsplit(xml_text(xml_find_all(featureMembers, ".//gml:pos")), " ")
    siteLocs <- data.frame(matrix(unlist(siteLocs), nrow=length(siteLocs), byrow=TRUE), stringsAsFactors = FALSE)
    names(siteLocs) <- c("dec_lat_va", "dec_lon_va")
    siteLocs <- mutate(siteLocs, dec_lat_va=as.numeric(dec_lat_va), dec_lon_va=as.numeric(dec_lon_va))
    #siteLocs <- data.frame(dec_lat_va=as.numeric(siteLocs[[1]][1]), dec_lon_va=as.numeric(siteLocs[[1]][2]), stringsAsFactors = FALSE)
    mergedDF <- cbind.data.frame(site, description = siteDesc, siteLocs, stringsAsFactors = FALSE) 
  }
  else{
    stop("Unrecognized response from the web service")
  }
  return(mergedDF)
}
