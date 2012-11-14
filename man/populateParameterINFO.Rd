\name{populateParameterINFO}
\alias{populateParameterINFO}
\title{Populate Parameter Information Columns}
\usage{
  populateParameterINFO(INFO, parameterCd,
    interactive = TRUE)
}
\arguments{
  \item{INFO}{dataframe with value and code columns}

  \item{parameterCd}{string USGS parameter code}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  INFO dataframe
}
\description{
  Populates INFO data frame with additional user-supplied
  information concerning the measured parameter.
}
\examples{
#This example requires an internet connection to run
exINFO <- getSiteFileData('01594440')
parameterCd <- "00175"
parameterData <- getParameterInfo(parameterCd,interactive=interactive)
exINFO$param.nm <- parameterData$parameter_nm
exINFO$param.units <- parameterData$parameter_units
exINFO$paramShortName <- parameterData$srsname
exINFO$paramNumber <- parameterData$parameter_cd
populateParameterINFO(exINFO, parameterCd,interactive=FALSE)
}

