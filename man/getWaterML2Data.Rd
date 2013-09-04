\name{getWaterML2Data}
\alias{getWaterML2Data}
\title{Function to return data from the WaterML2 data}
\usage{
  getWaterML2Data(obs_url)
}
\arguments{
  \item{obs_url}{string containing the url for the
  retrieval}
}
\value{
  mergedDF a data frame containing columns agency, site,
  dateTime, values, and remark codes for all requested
  combinations
}
\description{
  This function accepts a url parameter for a WaterML2
  getObservation
}
\examples{
url <- "http://webvastage6.er.usgs.gov/ogc-swie/wml2/uv/sos?request=GetObservation&featureID=01446500&observedProperty=00065&offering=UNIT&beginPosition=2013-08-20"
dataReturned <- getWaterML2Data(urlMulti)
}

