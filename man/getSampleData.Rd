\name{getSampleData}
\alias{getSampleData}
\title{Import Sample Data for WRTDS}
\usage{
  getSampleData(siteNumber, ParameterCd, StartDate,
    EndDate, interactive = TRUE)
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
Sample_01075 <- getSampleData('01594440','01075', '1985-01-01', '1985-03-31', interactive=FALSE)
Sample_All <- getSampleData('05114000','00915;00931', '1985-01-01', '1985-03-31', interactive=FALSE)
Sample_Select <- getSampleData('05114000','00915;00931', '', '', interactive=FALSE)
}
\seealso{
  \code{\link{compressData}},
  \code{\link{populateSampleColumns}}
}
\keyword{data}
\keyword{import}
\keyword{USGS}
\keyword{WRTDS}

