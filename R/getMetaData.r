#' Import Metadata for USGS Data
#'
#' This function is being deprecated for \code{\link{getNWISInfo}}.
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param parameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service WRTDS
#' @export
#' @return INFO dataframe with agency, site, dateTime, value, and code columns
#' @examples
#' # These examples require an internet connection to run
#' # Automatically gets information about site 05114000 and temperature, no interaction with user
#' INFO <- getMetaData('05114000','00010')
getMetaData <- function(siteNumber="", parameterCd="",interactive=TRUE){
  
  warning("This function is being deprecated, please use getNWISInfo")
  
  if (nzchar(siteNumber)){
    INFO <- getNWISSiteInfo(siteNumber)
  } else {
    INFO <- as.data.frame(matrix(ncol = 2, nrow = 1))
    names(INFO) <- c('site.no', 'shortName')    
  }
  INFO <- populateSiteINFO(INFO, siteNumber, interactive=interactive)
  
  if (nzchar(parameterCd)){
    parameterData <- getNWISPcodeInfo(parameterCd,interactive=interactive)
    INFO$param.nm <- parameterData$parameter_nm
    INFO$param.units <- parameterData$parameter_units
    INFO$paramShortName <- parameterData$srsname
    INFO$paramNumber <- parameterData$parameter_cd
  } 
  
  INFO <- populateParameterINFO(parameterCd, INFO, interactive=interactive)
  INFO$paStart <- 10
  INFO$paLong <- 12
  
  return(INFO)
}

#' Import Metadata for USGS Data
#'
#' Populates INFO data frame for EGRET study.  If either station number or parameter code supplied, imports data about a particular USGS site from NWIS web service. 
#' This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' If either station number or parameter code is not supplied, the user will be asked to input data.
#' Additionally, the user will be asked for:
#' staAbbrev - station abbreviation, will be used in naming output files and for structuring batch jobs
#' constitAbbrev - constitute abbreviation
#'
#' @param siteNumber string USGS site number.  This is usually an 8 digit number
#' @param parameterCd string USGS parameter code.  This is usually an 5 digit number.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service WRTDS
#' @export
#' @return INFO dataframe with at least param.nm, param.units, parameShortName, paramNumber
#' @examples
#' # These examples require an internet connection to run
#' # Automatically gets information about site 05114000 and temperature, no interaction with user
#' INFO <- getNWISInfo('05114000','00010')
getNWISInfo <- function(siteNumber, parameterCd,interactive=TRUE){
  if (nzchar(siteNumber)){
    INFO <- getNWISSiteInfo(siteNumber)
  } else {
    INFO <- as.data.frame(matrix(ncol = 2, nrow = 1))
    names(INFO) <- c('site.no', 'shortName')    
  }
  INFO <- populateSiteINFO(INFO, siteNumber, interactive=interactive)
  
  if (nzchar(parameterCd)){
    parameterData <- getNWISPcodeInfo(parameterCd,interactive=interactive)
    INFO$param.nm <- parameterData$parameter_nm
    INFO$param.units <- parameterData$parameter_units
    INFO$paramShortName <- parameterData$srsname
    INFO$paramNumber <- parameterData$parameter_cd
  } 
  
  INFO <- populateParameterINFO(parameterCd, INFO, interactive=interactive)
  INFO$paStart <- 10
  INFO$paLong <- 12
  
  return(INFO)
}

#' Import Metadata for Water Quality Portal Data
#'
#' Populates INFO data frame for EGRET study.  If either station number or parameter code supplied, imports data about a particular USGS site from NWIS web service. 
#' This function gets the data from here: \url{http://waterservices.usgs.gov/}
#' A list of parameter codes can be found here: \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/}
#' If either station number or parameter code is not supplied, the user will be asked to input data.
#' Additionally, the user will be asked for:
#' staAbbrev - station abbreviation, will be used in naming output files and for structuring batch jobs
#' constitAbbrev - constitute abbreviation
#'
#' @param siteNumber string site number. 
#' @param parameterCd string USGS parameter code or characteristic name.
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @keywords data import USGS web service WRTDS
#' @export
#' @return INFO dataframe with agency, site, dateTime, value, and code columns
#' @examples
#' # These examples require an internet connection to run
#' # Automatically gets information about site 01594440 and temperature, no interaction with user
#' nameToUse <- 'Specific conductance'
#' pcodeToUse <- '00095'
#' INFO <- getWQPInfo('USGS-04024315',pcodeToUse,interactive=TRUE)
#' INFO2 <- getWQPInfo('WIDNR_WQX-10032762',nameToUse)
#' # To adjust the label names:
#' INFO$shortName <- "Little"
#' INFO$paramShortName <- "SC"
getWQPInfo <- function(siteNumber, parameterCd, interactive=FALSE){
  
  #Check for pcode:
  pCodeLogic <- (all(nchar(parameterCd) == 5) & suppressWarnings(all(!is.na(as.numeric(parameterCd)))))

  if (pCodeLogic){
    
    siteInfo <- getWQPSites(siteid=siteNumber, pCode=parameterCd)

    parameterData <- getNWISPcodeInfo(parameterCd = parameterCd)
    
    siteInfo$param.nm <- parameterData$parameter_nm
    siteInfo$param.units <- parameterData$parameter_units
    siteInfo$paramShortName <- parameterData$srsname
    siteInfo$paramNumber <- parameterData$parameter_cd
    siteInfo$constitAbbrev <- parameterData$parameter_cd

  } else {
    siteInfo <- getWQPSites(siteid=siteNumber, characteristicName=parameterCd)

    siteInfo$param.nm <- parameterCd
    siteInfo$param.units <- ""
    siteInfo$paramShortName <- parameterCd
    siteInfo$paramNumber <- ""
    siteInfo$constitAbbrev <- parameterCd
  }
  
  siteInfo$station.nm <- siteInfo$MonitoringLocationName
  siteInfo$shortName <- siteInfo$station.nm 
  siteInfo$site.no <- siteInfo$MonitoringLocationIdentifier
  
  if(interactive){
    cat("Your site for data is", as.character(siteInfo$site.no),".\n")
    if (!nzchar(siteInfo$station.nm)){
      cat("No station name was listed for site: ", siteInfo$site.no, ". Please enter a station name here(no quotes): \n")
      siteInfo$station.nm <- readline()
    }
    cat("Your site name is", siteInfo$station.nm,",")
    cat("but you can modify this to a short name in a style you prefer. \nThis name will be used to label graphs and tables. \n")
    cat("If you want the program to use the name given above, just do a carriage return, otherwise enter the preferred short name(no quotes):\n")
    siteInfo$shortName <- readline()
    if (!nzchar(siteInfo$shortName)) siteInfo$shortName <- siteInfo$station.nm
    
    cat("Your water quality data are for parameter number", siteInfo$paramNumber, "which has the name:'", siteInfo$param.nm, "'.\n")
    cat("Typically you will want a shorter name to be used in graphs and tables. The suggested short name is:'", siteInfo$paramShortName, "'.\n")
    cat("If you would like to change the short name, enter it here, otherwise just hit enter (no quotes):")
    shortNameTemp <- readline()
    if (nchar(shortNameTemp)>0) siteInfo$paramShortName <- shortNameTemp
    cat("The units for the water quality data are: ", siteInfo$param.units, ".\n")
    cat("It is helpful to set up a constiuent abbreviation when doing multi-constituent studies, enter a unique id (three or four characters should work something like tn or tp or NO3).\nIt is case sensitive.  Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
    siteInfo$constitAbbrev <- readline()
  }
  
  if (interactive){
    cat("It is helpful to set up a station abbreviation when doing multi-site studies, enter a unique id (three or four characters should work).\nIt is case sensitive.  Even if you don't feel you need an abbreviation for your site you need to enter something(no quotes):\n")
    siteInfo$staAbbrev <- readline()
  } else {
    siteInfo$staAbbrev <- NA
  }

  if(siteInfo$DrainageAreaMeasure.MeasureUnitCode == "sq mi"){
    siteInfo$drainSqKm <- as.numeric(siteInfo$DrainageAreaMeasure.MeasureValue) * 2.5899881 
  } else {
    warning("Please check the units for drainage area. The value for INFO$drainSqKm needs to be in square kilometers,")
    siteInfo$drainSqKm <- as.numeric(siteInfo$DrainageAreaMeasure.MeasureValue)
  }
  
  if(interactive){
    if(is.na(siteInfo$drainSqKm)){
      cat("No drainage area was listed in the USGS site file for this site.\n")
      cat("Please enter the drainage area, you can enter it in the units of your choice.\nEnter the area, then enter drainage area code, \n1 is square miles, \n2 is square kilometers, \n3 is acres, \n4 is hectares.\n")
      cat("Area(no quotes):\n")
      siteInfo$drain.area.va <- readline()
      siteInfo$drain.area.va <- as.numeric(siteInfo$drain.area.va)
      cat("Unit Code (1-4, no quotes):")
      qUnit <- readline()
      qUnit <- as.numeric(qUnit)
      conversionVector <- c(2.5899881, 1.0, 0.0040468564, 0.01)
      siteInfo$drainSqKm <- siteInfo$drain.area.va * conversionVector[qUnit]
    }
  }
  
  siteInfo$queryTime <- Sys.time()
  siteInfo$paStart <- 10
  siteInfo$paLong <- 12
  
  return(siteInfo)
}

