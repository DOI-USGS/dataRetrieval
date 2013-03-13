\name{getDVData}
\alias{getDVData}
\title{Import Daily Data for WRTDS}
\usage{
  getDVData(siteNumber, ParameterCd, StartDate, EndDate,
    interactive = TRUE, convert = TRUE)
}
\arguments{
  \item{siteNumber}{string USGS site number.  This is
  usually an 8 digit number}

  \item{ParameterCd}{string USGS parameter code.  This is
  usually an 5 digit number.}

  \item{StartDate}{string starting date for data retrieval
  in the form YYYY-MM-DD.}

  \item{EndDate}{string ending date for data retrieval in
  the form YYYY-MM-DD.}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}

  \item{convert}{logical Option to include a conversion
  from cfs to cms (35.314667). The default is TRUE, which
  is appropriate for using NWIS data in the EGRET package.
  Set this to FALSE to not include the conversion.}
}
\value{
  Daily dataframe
}
\description{
  Imports data from NWIS web service. This function gets
  the data from here: \url{http://waterservices.usgs.gov/}
  A list of parameter codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/} A list
  of statistic codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
}
\examples{
# These examples require an internet connection to run
Daily <- getDVData('01594440','00060', '1985-01-01', '1985-03-31', interactive=FALSE)
}
\seealso{
  \code{\link{retrieveNWISData}},
  \code{\link{populateDaily}}
}
\keyword{data}
\keyword{import}
\keyword{USGS}
\keyword{WRTDS}

