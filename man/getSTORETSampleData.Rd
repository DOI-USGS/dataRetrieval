\name{getSTORETSampleData}
\alias{getSTORETSampleData}
\title{Import Sample Data for WRTDS}
\usage{
  getSTORETSampleData(siteNumber, characteristicName,
    StartDate, EndDate, interactive = TRUE)
}
\arguments{
  \item{siteNumber}{string site number.  If USGS, it should
  be in the form :'USGS-XXXXXXXXX...'}

  \item{characteristicName}{string}

  \item{StartDate}{string starting date for data retrieval
  in the form YYYY-MM-DD.}

  \item{EndDate}{string ending date for data retrieval in
  the form YYYY-MM-DD.}

  \item{interactive}{logical Option for interactive mode.
  If true, there is user interaction for error handling and
  data checks.}
}
\value{
  Sample dataframe
}
\description{
  Imports data from NWIS web service. This function gets
  the data from here: \url{http://qwwebservices.usgs.gov/}
  A list of parameter codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/pmcodes/} A list
  of statistic codes can be found here:
  \url{http://nwis.waterdata.usgs.gov/nwis/help/?read_file=stat&format=table}
  For raw data, use getQWData.  This function will retrieve
  the raw data, and compress it (summing constituents). See
  chapter 7 of the EGRET user guide for more details.
}
\examples{
# These examples require an internet connection to run
Sample_01075 <- getSTORETSampleData('USGS-01594440','Chloride', '', '', interactive=FALSE)
Sample_All <- getSTORETSampleData('WIDNR_WQX-10032762','Specific conductance', '', '', interactive=FALSE)
}
\seealso{
  \code{\link{compressData}},
  \code{\link{populateSampleColumns}}
}
\keyword{data}
\keyword{import}
\keyword{USGS}
\keyword{WRTDS}

