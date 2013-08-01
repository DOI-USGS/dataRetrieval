#' Populate Parameter Information Columns
#'
#' Populates INFO data frame with additional user-supplied information concerning the measured parameter.
#'
#' @param localINFO dataframe with value and code columns. Default is INFO
#' @param parameterCd string USGS parameter code
#' @param interactive logical Option for interactive mode.  If true, there is user interaction for error handling and data checks.
#' @return localINFO dataframe
#' @export
#' @examples
#' #This example requires an internet connection to run
#' INFO <- getSiteFileData('01594440')
#' parameterCd <- "01075"
#' parameterData <- getParameterInfo(parameterCd)
#' INFO$param.nm <- parameterData$parameter_nm
#' INFO$param.units <- parameterData$parameter_units
#' INFO$paramShortName <- parameterData$srsname
#' INFO$paramNumber <- parameterData$parameter_cd
#' INFO <- populateParameterINFO(parameterCd,interactive=FALSE)
populateParameterINFO <- function(parameterCd, localINFO=INFO, interactive=TRUE){
  if (nzchar(parameterCd)){
    if(interactive){
      message("Your water quality data are for parameter number", localINFO$paramNumber, "which has the name:'", localINFO$param.nm, "'.\n")
      message("Typically you will want a shorter name to be used in graphs and tables. The suggested short name is:'", localINFO$paramShortName, "'.\n")
      message("If you would like to change the short name, enter it here, otherwise just hit enter (no quotes):")
      shortNameTemp <- readline()
      if (nchar(shortNameTemp)>0) localINFO$paramShortName <- shortNameTemp
      message("The units for the water quality data are: ", localINFO$param.units, ".\n")
      message("It is helpful to set up a constiuent abbreviation when doing multi-constituent studies, enter a unique id (three or four characters should work something like tn or tp or NO3).\nIt is case sensitive.  Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
      localINFO$constitAbbrev <- readline()
    } else {
      localINFO$constitAbbrev <- localINFO$paramShortName
    }
  } else {
    if (interactive){
      localINFO$paramNumber <- NA
      message("Enter a long name for the water quality data (no quotes):\n")
      localINFO$param.nm <- readline()
      message("Enter a short name to be used in graphs and tables(no quotes):\n")
      localINFO$paramShortName <- readline()
      message("It is helpful to set up a constiuent abbreviation when doing multi-constituent studies, enter a unique id (three or four characters should work something like tn or tp or NO3).\nIt is case sensitive.  Even if you don't feel you need an abbreviation you need to enter something (no quotes):\n")
      localINFO$constitAbbrev <- readline()
      message("Enter the units of the water quality data(no quotes):\n")
      localINFO$param.units <- readline()
    } else {
      localINFO$paramNumber <- NA
      localINFO$param.nm <- NA
      localINFO$paramShortName <- NA
      localINFO$constitAbbrev <- NA
      localINFO$param.units <- NA      
    }
  } 
  
  return(localINFO)
}
