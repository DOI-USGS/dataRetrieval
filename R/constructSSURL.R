#' Construct Stream Stats url for data retrieval
#'
#' Imports data from StreamsStats web service. This function gets the data from here: \url{http://streamstatsags.cr.usgs.gov/ss_ws_92/Service.asmx?op=getStreamstats}
#' A list of parameter codes can be found here: \url{http://streamstatsags.cr.usgs.gov/webservices/wsui.htm}
#' The parameter codes and definations used by the StreamStats Web Service are used in this function
#' Not all states have StreamStats Web Services available. Check here for StreamStats Web Service Status:\url{http://streamstatsags.cr.usgs.gov/webservices/}
#' As of 2013-08-01, coordinates for the point to be delinated must be on the flow lines of the 
#' 100K NHD. If the coordinates are lnot on the 100K NHD flowlines, the delineated watershed will
#' must likely be for a very small area and the data wil most likely be erroneous 
#' 
#'
#' @param x (decimal degrees) the X coordinate or Longitude of the point to be delineated.
#' @param y (decimal degrees) the Y coordinate or Latitude of the point to be delineated.
#' @param inCRS (String) The Coordinate Reference System of the X and Y points in the format: EPSG:6.6:4269 
#' This format is defined by the Open Geospatial Consortium, Inc. and follows 
#' European Petroleum Survey Group: EPSG Geodesy Parameters V 6.6, available through www.epsg.org. 
#' An updated list of all available values can be found at \url{http://www.epsg-registry.org/}.
#' Some common values are: EPSG:6.6:4269 (NAD83 Geographic), EPSG:6.6:4326 (WGS 84 Gepgraphic) and EPSG:6.6:26913 (NAD83 UTM Zone 13)
#' @param StateNameAbbr (String) The FIPS State Alpha Code as defined by FIPSPUB 5-2: \url{http://www.itl.nist.gov/fipspubs/fip5-2.htm}
#' @param getBasinChars (String) Current valid values are C, CR, True, and False. C, CR, and True will compute the basin characteristics, 
#' temporarily storing them in the StreamStats System for download.  CR will return the results in XML format.
#' @param getFLowStats (String) Current valid values are C, CR, True, and False. C, CR, and 
#' True will compute the Streamflow Statistics, temporarily storing them in the StreamStats System for download.
#' CR return the results in XML format. 
#' @param getGeometry (String) Current valid values are KML, True, and False. True will return the geometry of 
#' the delineated watershed in a serialized ESRI PolygonN in the coordinate system defined by inCRS. KML will 
#' return a point centroid and a polygon in Keyhole Markup Language. All coordinates for KML will be output in WGS84
#' @param downLoadFeature (String) Current valid values are SHP (for downloading a shapefile), 
#' PGDB (for a Personal Geodatabase) or False. Any other value will default to False (do not download). 
#' @param clientID (String) A value to be temporarily stored in the NAME field of a delineated watershed. If a 
#' user were to put a unique identifier as the clientID, this value could then be used to tie the web service 
#' request to other user information.
#' @keywords data import USGS web service
#' @return url string
#' @export
#' @import RCurl
#' @examples
#' x <- -123.91492
#' y <- 45.00872
#' inCRS <- 'EPSG:6.6:4269'
#' StateNamedAbbr <- 'OR'
#' getBasinChars <- 'CR' 
#' getFlowStats <- 'CR'
#' getGeometry <- 'KML'
#' downLoadFeature <- 'False'
#' clientID <- 'You'
#' ss_url <- constructSSURL(x,y=,inCRS,StateNameAbbr,getBasinChars,getFlowStats,getGeometry,downLoadFeature,clientID)
constructSSURL <- function(x,y,inCRS,StateNameAbbr,getBasinChars,getFlowStats,getGeometry,downLoadFeature,clientID,str_ws_url="http://streamstatsags.cr.usgs.gov/",str_prefix="ss_ws_92/Service.asmx/getStreamstats?") {
  str_query <- paste0("x=",as.character(x), 
                      "&y=",as.character(y),
                      "&inCRS=",inCRS,
                      "&StateNameAbbr=",StateNameAbbr,
                      "&getBasinChars=",getBasinChars,
                      "&getFlowStats=",getFlowStats,
                      "&getGeometry=",getGeometry,
                      "&downloadFeature=", downLoadFeature,
                      "&clientID=",clientID)
  ss_url <- paste0(str_ws_url,str_prefix,str_query)
  return(ss_url)
  
}
