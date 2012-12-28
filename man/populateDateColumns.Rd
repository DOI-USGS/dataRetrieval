\name{populateDateColumns}
\alias{populateDateColumns}
\title{Populate Date Columns}
\usage{
  populateDateColumns(rawData)
}
\arguments{
  \item{rawData}{vector with dateTime}
}
\value{
  DateFrame dataframe
}
\description{
  Creates various date columns for WRTDS study.
}
\examples{
dateTime <- c('1985-01-01', '1985-01-02', '1985-01-03')
expandedDateDF <- populateDateColumns(dateTime)
}

