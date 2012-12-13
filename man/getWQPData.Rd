\name{getWQPData}
\alias{getWQPData}
\title{Data Import from Water Quality Portal}
\usage{
  getWQPData(siteNumber, characteristicName, StartDate,
    EndDate, interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string site number.  If USGS, it should
  be in the form :'USGS-XXXXXXXXX...'}

  \item{characteristicName}{string USGS parameter code.
  Multiple names can be input with a ';' separator.
  Leaving this blank will return all of the measured values
  during the specified time period.}

  \item{StartDate}{string starting date for data retrieval
  in the form YYYY-MM-DD.}

  \item{EndDate}{string ending date for data retrieval in
  the form YYYY-MM-DD.}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  retval dataframe with first column dateTime, and at least
  one qualifier and value columns (subsequent
  qualifier/value columns could follow depending on
  requested parameter codes)
}
\description{
  Imports data from Water Quality Portal web service. This
  function gets the data from here:
  \url{http://www.waterqualitydata.us}. This function is
  more general than getQWData because it allows for other
  agencies rather than the USGS.  Therefore, the 5-digit
  parameter code cannot be used. Instead, this function
  uses characteristicName.  A complete list can be found
  here
  \url{http://www.waterqualitydata.us/Codes/Characteristicname}
}
\examples{
# These examples require an internet connection to run
\dontrun{getWQPData('USGS-01594440','Chloride', '', '')}
\dontrun{getWQPData('USGS-05114000','Suspended Sediment Discharge;Suspended sediment concentration (SSC);Suspended sediment discharge', '1975-01-01', '1995-03-31')}
\dontrun{getWQPData('USGS-05114000','00915;00931', '1985-01-01', '1985-04-30', interactive=FALSE)}
}
\keyword{WQP}
\keyword{data}
\keyword{import}
\keyword{service}
\keyword{web}

