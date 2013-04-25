#' Populate Site Information Columns
#'
#' Populates INFO data frame with additional user-supplied information. Also removes fields not related to WRTDS study.
#'
#' @param INFO dataframe with value and code columns
#' @param siteNumber string USGS site number
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @return INFO dataframe
#' @export
#' @examples
#' #This example requires an internet connection to run
#' INFO <- getSiteFileData('01594440')
#' siteNumber <- "01594440"
#' siteINFO <- populateSiteINFO(INFO, siteNumber,interactive=FALSE)
populateSiteINFO <- function(INFO, siteNumber,interactive=TRUE){
  if (nzchar(siteNumber)){
    
    INFO$land.net.ds <- NULL
    INFO$instruments.cd <- NULL
    INFO$nat.aqfr.cd <- NULL
    INFO$aqfr.cd <- NULL
    INFO$aqfr.type.cd <- NULL
    INFO$well.depth.va <- NULL
    INFO$hole.depth.va <- NULL
    INFO$hole.depth.va <- NULL
    INFO$depth.src.cd <- NULL
    INFO$gw.file.cd <- NULL
    
    if (!nzchar(INFO$site.no)) {
      INFO$site.no <- siteNumber
    }
    
    if (interactive){
      cat("Your site for streamflow data is", as.character(INFO$site.no),".\n")
      if (!nzchar(INFO$station.nm)){
        cat("No station name was listed in the USGS site file for site: ", INFO$site.no, ". Please enter a station name here(no quotes): \n")
        INFO$station.nm <- readline()
      }
      cat("Your site name is", INFO$station.nm,",")
      cat("but you can modify this to a short name in a style you prefer. \nThis name will be used to label graphs and tables. \n")
      cat("If you want the program to use the name given above, just do a carriage return, otherwise enter the preferred short name(no quotes):\n")
      INFO$shortName <- readline()
      if (!nzchar(INFO$shortName)) INFO$shortName <- INFO$station.nm
      if (!nzchar(INFO$dec.lat.va) || !nzchar(INFO$dec.long.va)){
        cat("No latitude or longitude was listed in the USGS site file for this site.\n")
        cat("Please enter a latitude and longitude in decimal degrees, positive latitudes are north, negative are south, positive longitudes are east, \nnegative longitudes are west, so for example a site in the northeastern US might look like, 40.1, -83.2\nThese only need to be sufficiently accurate to place them on a map of the study area.\n\n")
        cat("Latitude(no quotes):\n")
        INFO$dec.lat.va <- readline()
        cat("Longitude(no quotes):\n")
        INFO$dec.long.va <- readline()
      }
      cat("The latitude and longitude of the site are: ",INFO$dec.lat.va, ", ", INFO$dec.long.va, "(degrees north and west).\n")
      if (!nzchar(INFO$drain.area.va)){
        cat("No drainage area was listed in the USGS site file for this site.\n")
        cat("Please enter the drainage area, you can enter it in the units of your choice.\nEnter the area, then enter drainage area code, 1 is square miles, 2 is square kilometers, 3 is acres, and 4 is hectares.\n")
        cat("Area(no quotes):\n")
        INFO$drain.area.va <- readline()
        INFO$drain.area.va <- as.numeric(INFO$drain.area.va)
        cat("Unit Code (1-4, no quotes):")
        qUnit <- readline()
        qUnit <- as.numeric(qUnit)
        conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
        INFO$drainSqKm <- INFO$drain.area.va * conversionVector[qUnit]
      } else {
        INFO$drain.area.va <- as.numeric(INFO$drain.area.va)
        INFO$contrib.drain.area.va <- as.numeric(INFO$contrib.drain.area.va)
        INFO$drainSqKm <- INFO$drain.area.va * 2.5899881
      }    
      cat("The drainage area at this site is ", INFO$drain.area.va, "square miles which is being stored as", INFO$drainSqKm, "square kilometers.\n")    
    } else {
      INFO$drain.area.va <- as.numeric(INFO$drain.area.va)
      INFO$contrib.drain.area.va <- as.numeric(INFO$contrib.drain.area.va)
      INFO$drainSqKm <- INFO$drain.area.va * 2.5899881      
      INFO$shortName <- INFO$station.nm
    }    
  } else {
    if (interactive){
      cat("The program needs to know a site number or id, please enter that here (don't use quotes) - Enter to leave blank:")
      INFO$site.no <- readline()
      cat("Please enter a site name that will be used to label all graphs and tables(no quotes):\n")
      INFO$shortName <- readline()
      cat("Please enter a latitude and longitude in decimal degrees, positive latitudes are north, negative are south, positive longitudes are east, \nnegative longitudes are west, so for example a site in the northeastern US might look like, 40.1, -83.2\nThese only need to be sufficiently accurate to place them on a map of the study area.\n\n")
      cat("Latitude(no quotes):\n")
      INFO$dec.lat.va <- readline()
      cat("Longitude(no quotes):\n")
      INFO$dec.long.va <- readline()
      INFO$dec.lat.va <- as.numeric(INFO$dec.lat.va)
      INFO$dec.long.va <- as.numeric(INFO$dec.long.va)
      cat("Please enter the drainage area, you can enter it in the units of your choice.\nEnter the area, then enter drainage area code, 1 is square miles, 2 is square kilometers, 3 is acres, and 4 is hectares.\n")
      cat("Area(no quotes):\n")
      INFO$drain.area.va <- readline()
      INFO$drain.area.va <- as.numeric(INFO$drain.area.va)
      cat("Unit Code (1-4, no quotes):")
      qUnit <- readline()
      qUnit <- as.numeric(qUnit)
      conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
      INFO$drainSqKm <- INFO$drain.area.va * conversionVector[qUnit]
      cat("The drainage area is being stored as", INFO$drainSqKm, "square kilometers.\n")
    } else {
      INFO$site.no <- NA
      INFO$shortName <- NA
      INFO$dec.lat.va <- NA
      INFO$dec.long.va <- NA
      INFO$drain.area.va <- NA
      INFO$drainSqKm <- NA
    }
  }
  if (interactive){
    cat("It is helpful to set up a station abbreviation when doing multi-site studies, enter a unique id (three or four characters should work).\nIt is case sensitive.  Even if you don't feel you need an abbreviation for your site you need to enter something(no quotes):\n")
    INFO$staAbbrev <- readline()
  } else {
    INFO$staAbbrev <- NA
  }
  return(INFO)  
}
