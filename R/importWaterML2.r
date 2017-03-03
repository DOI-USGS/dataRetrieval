#' Function to return data from the WaterML2 data
#'
#' This function accepts a url parameter for a WaterML2 getObservation. This function is still under development,
#' but the general functionality is correct.
#'
#' @param obs_url character or raw, containing the url for the retrieval or a path to the data file, or raw XML.
#' @param asDateTime logical, if \code{TRUE} returns date and time as POSIXct, if \code{FALSE}, character
#' @param tz character to set timezone attribute of datetime. Default converts the datetimes to UTC 
#' (properly accounting for daylight savings times based on the data's provided tz_cd column).
#' Recommended US values include "UTC","America/New_York","America/Chicago", "America/Denver","America/Los_Angeles",
#' "America/Anchorage","America/Honolulu","America/Jamaica","America/Managua","America/Phoenix", and "America/Metlakatla".
#' For a complete list, see \url{https://en.wikipedia.org/wiki/List_of_tz_database_time_zones}
#' @return mergedDF a data frame time, value, description, qualifier, and identifier
#' @export
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_text
#' @importFrom xml2 xml_attr
#' @importFrom dplyr rbind_all select
#' @importFrom lubridate parse_date_time
#' @examples
#' baseURL <- "https://waterservices.usgs.gov/nwis/dv/?format=waterml,2.0"
#' URL <- paste(baseURL, "sites=01646500",
#'      "startDT=2014-09-01",
#'      "endDT=2014-09-08",
#'      "statCd=00003",
#'      "parameterCd=00060",sep="&")
#' \dontrun{
#' dataReturned1 <- importWaterML2(URL)
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
importWaterML2 <- function(obs_url, asDateTime=FALSE, tz="UTC"){
  
  if(tz != ""){
    tz = "UTC"
  }
  tz <- match.arg(tz, OlsonNames())
  
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
    
    df <- parseWaterML2Timeseries(t, asDateTime)
    #need to save attributes first, and create identifier column
    saveAttribs <- attributes(df)[-(1:3)]
    #remove time and date columns, add site col 
    df <- mutate(df, identifier = saveAttribs$gmlID, 
                 qualifier = ifelse(is.null(saveAttribs$defaultQualifier), 
                                    NA, saveAttribs$defaultQualifier))
    if(all(is.na(df$dateTime))){
      df <- subset(df, select=-c(dateTime, time))
      #should the remaining column be changed to dateTime?
    } else {
      df <- subset(df, select=-c(date, time))
    }
    if (is.null(mergedDF)){
      mergedDF <- df
    } else {
      similarNames <- intersect(colnames(mergedDF), colnames(df))
      mergedDF <- full_join(mergedDF, df, by=similarNames)
    }
    attributes(mergedDF) <- append(attributes(mergedDF), saveAttribs)
  }
  return(mergedDF)
}


