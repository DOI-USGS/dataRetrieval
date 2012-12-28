\name{getMetaData}
\alias{getMetaData}
\title{Import Metadata for USGS Data}
\usage{
  getMetaData(siteNumber = "", parameterCd = "",
    interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string USGS site number.  This is
  usually an 8 digit number}

  \item{parameterCd}{string USGS parameter code.  This is
  usually an 5 digit number.}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  INFO dataframe with agency, site, dateTime, value, and
  code columns
}
\description{
  Populates INFO data frame for WRTDS study.  If either
  station number or parameter code supplied, imports data
  about a particular USGS site from NWIS web service. This
  function gets the data from here:
  \url{http://waterservices.usgs.gov/} A list of parameter
  codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/} If
  either station number or parameter code is not supplied,
  the user will be asked to input data. Additionally, the
  user will be asked for: staAbbrev - station abbreviation,
  will be used in naming output files and for structuring
  batch jobs constitAbbrev - constitute abbreviation
}
\examples{
# These examples require an internet connection to run
# Automatically gets information about site 05114000 and temperature, no interaction with user
INFO <- getMetaData('05114000','00010',interactive=FALSE)
}
\keyword{USGS}
\keyword{WRTDS}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

