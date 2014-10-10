#' Populate Parameter Information Columns
#'
#' Populates INFO data frame with additional user-supplied information concerning the measured parameter.
#'
#' @param INFO dataframe with value and code columns. Default is INFO
#' @param parameterCd string USGS parameter code
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @return INFO dataframe
#' @export
#' @examples
#' #This example requires an internet connection to run
#' INFO <- getNWISSiteInfo('01594440')
#' parameterCd <- "01075"
#' parameterData <- getNWISPcodeInfo(parameterCd)
#' INFO$param.nm <- parameterData$parameter_nm
#' INFO$param.units <- parameterData$parameter_units
#' INFO$paramShortName <- parameterData$srsname
#' INFO$paramNumber <- parameterData$parameter_cd
#' INFO <- populateParameterINFO(parameterCd, INFO)
populateParameterINFO <- function(parameterCd, INFO, interactive=TRUE){
  if (nzchar(parameterCd)){
    if(interactive){
      cat("Your water quality data are for parameter number", INFO$paramNumber, "which has the name:'", INFO$param.nm, "'.\n")
      cat("Typically you will want a shorter name to be used in graphs and tables. The suggested short name is:'", INFO$paramShortName, "'.\n")
      cat("If you would like to change the short name, enter it here, otherwise just hit enter (no quotes):")
      shortNameTemp <- readline()
      if (nchar(shortNameTemp)>0) INFO$paramShortName <- shortNameTemp
      cat("The units for the water quality data are: ", INFO$param.units, ".\n")
      cat("It is helpful to set up a constiuent abbreviation when doing multi-constituent studies, enter a unique id (three or four characters should work something like tn or tp or NO3).\nIt is case sensitive.  Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
      INFO$constitAbbrev <- readline()
    } else {
      INFO$constitAbbrev <- INFO$paramShortName
    }
  } else {
    if (interactive){
      INFO$paramNumber <- NA
      cat("Enter a long name for the water quality data (no quotes):\n")
      INFO$param.nm <- readline()
      cat("Enter a short name to be used in graphs and tables(no quotes):\n")
      INFO$paramShortName <- readline()
      cat("It is helpful to set up a constiuent abbreviation when doing multi-constituent studies, enter a unique id (three or four characters should work something like tn or tp or NO3).\nIt is case sensitive.  Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
      INFO$constitAbbrev <- readline()
      cat("Enter the units of the water quality data(no quotes):\n")
      INFO$param.units <- readline()
    } else {
      INFO$paramNumber <- NA
      INFO$param.nm <- NA
      INFO$paramShortName <- NA
      INFO$constitAbbrev <- NA
      INFO$param.units <- NA      
    }
  } 
  
  return(INFO)
}
